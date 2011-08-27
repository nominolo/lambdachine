#include "Common.h"

#if LC_HAS_JIT

#include "HeapInfo.h"
#include "PrintIR.h"

#include <string.h>

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
  u4 nfields = info->size;
  HeapInfo *hp = &J->cur.heap[J->cur.nheap++];
  hp->mapofs = J->cur.nheapmap;
  hp->ref = ref;
  hp->nfields = nfields;
  hp->nent = nfields;
  hp->dfs = hp->scc = 0;
  hp->loop = 0;
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
  hp->loop = 0;
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
printHeapInfo(FILE *stream, JitState *J)
{
  u4 i, j;
  for (i = 0; i < J->cur.nheap; i++) {
    HeapInfo *hp = &J->cur.heap[i];
    fprintf(stream, "  [%d,%d] %s ", hp->mapofs, hp->scc,
           (hp->loop & 1) ? "L" : " ");
    printIRRef(&J->cur, hp->ref);
    fprintf(stream, "=> ");
    for (j = 0; j < hp->nfields; j++) {
      printIRRef(&J->cur, getHeapInfoField(&J->cur, hp, j));
    }
    fprintf(stream, "\n");
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
  IRRef href;
  int i;

  DBG_LVL(3,"dfs1: %d\n", n);

  hp = HP(n);
  //if (n < 0 || n >= J->cur.nheap) return;
  if (marked(hp)) return;

  href = hp->ref;
  hp->loop = 0;

  //LC_ASSERT(!marked(hp));

  hp->dfs = D->pre++;
  pushStack(&D->s, n);
  pushStack(&D->p, n);

  // Traverse all children (may already be marked)
  for (i = 0; i < hp->nfields; i++) {
    IRRef ref = getHeapInfoField(&J->cur, hp, i);
    IRIns *ir;
    //printf("..%d -> %d?\n", n, ref - REF_BIAS);
    if (ref == 0) continue;
    if (ref == href)
      hp->loop |= 1;
    ir = IR(ref);
    if (ir->o == IR_NEW) {
      dfs2(J, D, ir->op2);
      IRRef other = findPhiTwin(J, ref);
      DBG_LVL(3,"..%d looking for phi twin of %d => %d\n", n, ref - REF_BIAS, other - REF_BIAS);
      if (other) {
	if (other == href)
	  hp->loop |= 1;
        DBG_LVL(3,"..%d following %d, %d\n", n,
                other - REF_BIAS, IR(other)->op2);
        dfs2(J, D, IR(other)->op2);
      }
    }
  }

  if (peekStack(&D->p) == n) {  // Found SCC
    popStack(&D->p);
    // We pop items off stack S until we found the current item.
    // If the first item is != n, then were found an SCC of size > 1,
    // so we mark all members as being part of a loop.
    u1 isloop = (peekStack(&D->s) != n) ? 1 : 0;
    u2 p;
    do {
      p = popStack(&D->s);
      DBG_LVL(3, "Adding item to SCC: %d, %d\n", n, p);
      HP(p)->scc = hp->dfs;
      HP(p)->loop |= isloop;
    } while (p != n);
  }
}

void
dfs2(JitState *J, SccData *D, u2 w)
{
  DBG_LVL(3,"dfs2: %d\n", w);
  HeapInfo *hp;

  if (w < 0 || w > J->cur.nheap) return;

  hp = HP(w);

  if (!marked(hp)) {
    dfs1(J, D, w);
  } else if (hp->scc == 0) {
    while (!isEmptyStack(&D->p) && HP(peekStack(&D->p))->dfs > hp->dfs) {
      popStack(&D->p);
    }
  }
}

#define UNSINK_VISITED   2
#define UNSINK_LOOP      1

void
markUnsinkable(JitState *J, u2 hi, u1 mode)
{
  HeapInfo *hp = HP(hi);

  if ((hp->loop & UNSINK_VISITED) && (hp->loop & UNSINK_LOOP))
    return;

  hp->loop |= UNSINK_VISITED | mode;
  DBG_LVL(2, "unsink %d, %d\n", hi, mode);

  if (hp->loop & UNSINK_LOOP) {
    int i;
    for (i = 0; i < hp->nfields; i++) {
      // Follow children
      IRRef twin, ref = getHeapInfoField(&J->cur, hp, i);
      IRIns *ir;

      if (ref == 0)
        continue;

      ir = IR(ref);
      if (ir->o == IR_NEW) {
        markUnsinkable(J, ir->op2, UNSINK_LOOP);

        twin = findPhiTwin(J, ref);
        if (twin)
          markUnsinkable(J, IR(twin)->op2, UNSINK_LOOP);
      }
    }
  }
}

void
heapSCCs(JitState *J)
{
  SccData D;
  int i;

  initStack(&D.s, J->cur.nheap);
  initStack(&D.p, J->cur.nheap);
  D.pre = 1;

  // 1. Find all cycles (including

  // Roots are all NEW instructions after the LOOP marker.
  // Start with last instruction.
  IRRef ref = J->chain[IR_NEW];
  while (ref > J->cur.nloop) {
    dfs1(J, &D, IR(ref)->op2);
    ref = IR(ref)->prev;
  }

  // 2. Mark all nodes reachable from cycles as unsinkable.
  //
  // Starting from the back again, because nodes from the back
  // may reach any other node, but not necessarily vice versa.
  for (i = J->cur.nheap - 1; i >= 0; i--) {
    markUnsinkable(J, i, 0);
  }

  destroyStack(&D.s);
  destroyStack(&D.p);
}

#undef HP
#undef IR

#endif  /* LC_HAS_JIT */
