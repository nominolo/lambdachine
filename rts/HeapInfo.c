#include "HeapInfo.h"
#include "PrintIR.h"

#include <strings.h>

// -- Convenience macros.  Undefined at end of file. -----------------

// Pointer to referenced IR.
#define IR(ref)     (&J->cur.ir[(ref)])

// -------------------------------------------------------------------

void
growHeapInfoBuffer_(JitState *J, Word needed)
{
  Word maxheap = JIT_MAXALLOCS;
  if (needed > maxheap)
    traceError(J, 214);

  J->heapbuf = realloc(J->heapbuf, maxheap * sizeof(HeapInfo));
  J->sizeheap = needed;
  J->cur.heap = J->heapbuf;
}

void
growHeapInfoMapBuffer_(JitState *J, Word needed)
{
  if (needed < 2 * J->sizeheapmap)
    needed = 2 * J->sizeheapmap;
  else if (needed < 64)
    needed = 64;

  J->heapmapbuf = realloc(J->heapmapbuf, needed * sizeof(HeapEntry));
  J->cur.heapmap = J->heapmapbuf;
  J->sizeheapmap = needed;
}

// Plan:
//
//  - The second field of an ILOAD and NEW instruction tells us
//    the index into the heap info map.
//
//  - For the purposes of detecting sinkable allocations, we only need
//    to keep track of pointer fields.
//
// Hypotheses:
//
//  - The IRRef of a NEW instruction already are in a valid
//    reverse depth-first ordering.
//

u4
newHeapInfo(JitState *J, IRRef1 ref, InfoTable *info)
{
  growHeapInfoBuffer(J, J->cur.nheap + 1);
  u4 nfields = info->layout.payload.ptrs + info->layout.payload.nptrs;
  HeapInfo *hp = &J->cur.heap[J->cur.nheap++];
  hp->mapofs = J->cur.nheapmap;
  hp->ref = ref;
  hp->nfields = nfields;
  hp->nent = (nfields + 1) / 2;
  hp->dfs = hp->scc = 0;
  growHeapInfoMapBuffer(J, J->cur.nheapmap + hp->nent);
  memset(J->cur.heapmap + hp->mapofs, 0, hp->nent * sizeof(HeapEntry));
  J->cur.nheapmap += hp->nent;
  return J->cur.nheap - 1;
}

u4
cloneHeapInfo(JitState *J, IRRef1 ref, u2 orig)
{
  HeapInfo *hp, *hporig;

  growHeapInfoBuffer(J, J->cur.nheap + 1);

  hporig = &J->cur.heap[orig];
  hp = &J->cur.heap[J->cur.nheap++];
  hp->mapofs = J->cur.nheapmap;
  hp->ref = ref;
  hp->nfields = hporig->nfields;
  hp->nent = hporig->nent;
  hp->dfs = hp->scc = 0;
  growHeapInfoMapBuffer(J, J->cur.nheapmap + hp->nent);
  memset(J->cur.heapmap + hp->mapofs, 0, hp->nent * sizeof(HeapEntry));
  J->cur.nheapmap += hp->nent;
  return J->cur.nheap - 1;
}

HeapInfo *
getHeapInfo(JitState *J, IRRef ref)
{
  IRIns *ir = IR(ref);
  switch (ir->o) {
  case IR_NEW:
    LC_ASSERT(ir->op2 < J->cur.nheap);
    return &J->cur.heap[ir->op2];
  default:
    LC_ASSERT(0); return NULL;
  }
}

void
printHeapInfo(JitState *J)
{
  u4 i, j;
  for (i = 0; i < J->cur.nheap; i++) {
    HeapInfo *hp = &J->cur.heap[i];
    printf("  [%d,%d] ", hp->mapofs, hp->scc);
    printIRRef(J, hp->ref);
    printf("=> ");
    for (j = 0; j < hp->nfields; j++) {
      HeapEntry *he = &J->cur.heapmap[hp->mapofs + (j >> 1)];
      IRRef1 ref = j & 1 ? *he >> 16 : *he & 0xffff;
      printIRRef(J, ref);
    }
    printf("\n");
  }
}

// -- Finding Strongly Connected Components --------------------------

void
resetForSCC(JitState *J)
{
  u4 i;
  for (i = 0; i < J->cur.nheap; i++) {
    J->cur.heap[i].dfs = 0;
    J->cur.heap[i].scc = 0;
  }
}

typedef u2 elem_type;

typedef struct {
  u4 top;
  elem_type *elems;
} Stack;

INLINE_HEADER void
initStack(Stack *s, u4 size)
{
  s->top = 0;
  s->elems = xmalloc(size * sizeof(elem_type));
}

INLINE_HEADER void
destroyStack(Stack *s)
{
  s->top = 0;
  xfree(s->elems);
}

INLINE_HEADER void
pushStack(Stack *s, elem_type data)
{
  s->elems[s->top++] = data;
}

INLINE_HEADER elem_type
peekStack(Stack *s)
{
  if (s->top == 0)
    return 0;
  else
    return s->elems[s->top - 1];
}

INLINE_HEADER elem_type
popStack(Stack *s)
{
  return s->elems[--s->top];
}

INLINE_HEADER int
isEmptyStack(Stack *s)
{
  return s->top == 0;
}

typedef struct {
  Stack p, s;
  int pre;
} SccData;

#define HP(x)   (&J->cur.heap[(x)])

INLINE_HEADER int marked(HeapInfo* hp)
{
  return hp->dfs != 0;
}

void dfs2(JitState *J, SccData *D, u2 w);

void
dfs1(JitState *J, SccData *D, u2 n)
{
  HeapInfo *hp;
  int i;

  if (n == 0) return;

  hp = HP(n);
  LC_ASSERT(!marked(hp));

  hp->dfs = D->pre++;
  pushStack(&D->s, n);
  pushStack(&D->p, n);

  // Traverse all children (may already be marked)
  for (i = 0; i < hp->nfields; i++) {
    HeapEntry hpe = J->cur.heapmap[hp->mapofs + i];
    IRIns *ir;
    if (heap_ref(hpe) == 0) continue;
    ir = IR(heap_ref(hpe));
    if (ir->o == IR_NEW) {
      dfs2(J, D, ir->op2);
    }
  }

  if (peekStack(&D->p) == n) {  // Found SCC
    popStack(&D->p);
    u2 p;
    do {
      p = popStack(&D->s);
      HP(p)->scc = hp->dfs;
    } while (p != n);
  }
}

void
dfs2(JitState *J, SccData *D, u2 w)
{
  HeapInfo *hp;

  if (w == 0) return;

  hp = HP(w);

  if (!marked(hp)) {
    dfs1(J, D, w);
  } else if (hp->scc == 0) {
    while (!isEmptyStack(&D->p) && HP(peekStack(&D->p))->dfs > hp->dfs) {
      popStack(&D->p);
    }
  }
}

void
heapSCCs(JitState *J, u2 root)
{
  SccData D;
  initStack(&D.s, J->cur.nheap);
  initStack(&D.p, J->cur.nheap);
  D.pre = 1;

  dfs1(J, &D, root);

  destroyStack(&D.s);
  destroyStack(&D.p);
}

#undef HP
#undef IR
