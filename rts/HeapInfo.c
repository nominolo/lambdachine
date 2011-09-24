#include "Common.h"

#if LC_HAS_JIT

#include "HeapInfo.h"
#include "PrintIR.h"
#include "IR.h"

#include <string.h>

// -- Convenience macros.  Undefined at end of file. -----------------

// Pointer to referenced IR.
#define IR(ref)     (&J->cur.ir[(ref)])

#define UNSINK_VISITED   2
#define UNSINK_LOOP      1

// -------------------------------------------------------------------
// Forward declarations

void debugIRDeps(JitState *J);

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
  hp->ind = 0;
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
  hp->ind = 0;
  growHeapInfoMapBuffer(J, J->cur.nheapmap + hp->nent);
  memset(J->cur.heapmap + hp->mapofs, 0, hp->nent * sizeof(HeapEntry));
  J->cur.nheapmap += hp->nent;
  return J->cur.nheap - 1;
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
//
// The goal is to find heap nodes contain a (transitive) reference to
// itself.  Such heap nodes must be allocated on-trace and cannot be 
// pushed into side exits.
// 
// Actually, this is a bit too conservative.  We only cannot sink
// allocations that depend on a previous version of itself.  In the IR
// this means that must be a chain of references from node N to itself
// that passes through a PHI node.  Of course, any node refenced from
// such a cycle also cannot be sunken, either.
//
// So, the purpose of the following code is to find cycles in
// references from NEW nodes.  We use the Cheriyan-Mehlhorn-Gabow
// algorithm to find strongly connected components (SCCs) in the
// heap graph.  Any SCC of size > 1 is marked as a loop and not
// sunken.
//
// If a thunk is updated with a new value, we track cycles through
// both the thunk and the updated value.  I.e., we union the references
// of both values.

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

#define HP(x)   (LC_ASSERT((x) < J->cur.nheap), &J->cur.heap[(x)])

INLINE_HEADER int marked(HeapInfo* hp)
{
  return hp->dfs != 0;
}

void dfs2(JitState *J, SccData *D, u2 w);

// Traverse a node that has not yet been processed.
//
// Only PHI and NEW nodes are interesting.
void
dfs1(JitState *J, SccData *D, u2 n)
{
  HeapInfo *hp, *hp1;
  IRRef href;
  int i;

  hp1 = hp = HP(n);

  DBG_LVL(3,"SCC: dfs1: HP(%d), %d\n", n, irref_int(hp->ref));
  //if (n < 0 || n >= J->cur.nheap) return;
  if (marked(hp)) return;

  href = hp->ref;
  hp->loop = 0;

  //LC_ASSERT(!marked(hp));

  hp->dfs = D->pre++;
  pushStack(&D->s, n);
  pushStack(&D->p, n);

  do {
    // Traverse all children (may already be marked)
    for (i = 0; i < hp->nfields; i++) {
      IRRef ref = getHeapInfoField(&J->cur, hp, i);
      IRIns *ir;

      DBG_PR("SCC: ..%d -> %d?\n", irref_int(href), irref_int(ref));

      if (ref == 0)
        continue;

      // This heap node contains a pointer to itself.  We do not
      // strictly need to mark this as a loop, but we'll do for now.
      if (ref == href)
        hp->loop |= UNSINK_LOOP;
      ir = IR(ref);
      if (ir->o == IR_NEW) {
        dfs2(J, D, ir->op2);
        IRRef other = findPhiTwin(J, ref);
        DBG_LVL(3,"SCC: ..HP(%d) looking for phi twin of %d => %d\n",
                n, irref_int(ref), irref_int(other));
        if (other) {
          if (other == href)
            hp->loop |= UNSINK_LOOP;
          DBG_LVL(3,"SCC: ..HP(%d) following %d, HP(%d)\n", n,
                  other - REF_BIAS, IR(other)->op2);
          dfs2(J, D, IR(other)->op2);
        }
      }
    }

    // Traverse all children of the node we were update with (if any)
    if (hp->ind && IR(hp->ind)->o == IR_NEW)
      hp = getHeapInfo(J, IR(hp->ind));
    else
      hp = NULL;
  } while (hp);

  if (peekStack(&D->p) == n) {  // Found SCC
    popStack(&D->p);
    // We pop items off stack S until we found the current item.
    // If the first item is != n, then we found an SCC of size > 1,
    // so we mark all members as being part of a loop.
    u1 isloop = (peekStack(&D->s) != n) ? 1 : 0;
    u2 p;
    do {
      p = popStack(&D->s);
      DBG_LVL(3, "SCC: Adding item to SCC: %d, %d\n", n, p);
      HP(p)->scc = hp1->dfs;
      HP(p)->loop |= isloop;
    } while (p != n);
  }
}

void
dfs2(JitState *J, SccData *D, u2 w)
{
  DBG_LVL(3,"SCC: dfs2: %d\n", w);
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

//--------------------------------------------------------------

LC_AINLINE u1
nodeDfs(JitState *J, IRRef ref)
{
  IRIns *ir = IR(ref);
  switch (ir->o) {
  case IR_NEW:
    return HP((u2)ir->op2)->dfs;
  case IR_PHI:
    return ir->r; // yes, quite a hack
  default:
    LC_ASSERT(0);
    fprintf(stderr, "nodeDfs: Wrong node type %d", ir->o);
    exit(2);
  }
}

LC_AINLINE void
setNodeDfs(JitState *J, IRRef ref, u1 dfs)
{
  IRIns *ir = IR(ref);
  switch (ir->o) {
  case IR_NEW:
    HP((u2)ir->op2)->dfs = dfs;
    break;
  case IR_PHI:
    ir->r = dfs;
    break;
  default:
    LC_ASSERT(0);
    fprintf(stderr, "setNodeDfs: Wrong node type %d", ir->o);
    exit(2);
  }
}

LC_AINLINE u1
nodeScc(JitState *J, IRRef ref)
{
  IRIns *ir = IR(ref);
  switch (ir->o) {
  case IR_NEW:
    return HP((u2)ir->op2)->scc;
  case IR_PHI:
    return ir->s; // and another hack
  default:
    LC_ASSERT(0);
    fprintf(stderr, "nodeDscc: Wrong node type %d", ir->o);
    exit(2);
  }
}

LC_AINLINE void
setNodeScc(JitState *J, IRRef ref, u1 scc)
{
  IRIns *ir = IR(ref);
  switch (ir->o) {
  case IR_NEW:
    HP((u2)ir->op2)->scc = scc;
    break;
  case IR_PHI:
    ir->s = scc;
    break;
  default:
    LC_ASSERT(0);
    fprintf(stderr, "nodeDscc: Wrong node type %d", ir->o);
    exit(2);
  }
}

LC_FASTCALL void dfs2a(JitState *J, SccData *D, IRRef ref);

void
dfs1a(JitState *J, SccData *D, IRRef ref)
{
  int i;
  IRRef ref0 = ref;

  DBG_LVL(3, "SCC: dfs1: %d\n", irref_int(ref));

  LC_ASSERT(nodeDfs(J, ref) == 0);  // not yet marked

  setNodeDfs(J, ref, D->pre++);
  pushStack(&D->s, ref);
  pushStack(&D->p, ref);

  for (;;) {
    IRIns *ir = IR(ref);

    if (ir->o == IR_NEW) {
      HeapInfo *hp = HP(ir->op2);

      DBG_LVL(3, "SCC: Traversing NEW node %d (size: %d)\n",
              irref_int(ref), hp->nfields);

      // Traverse all children (may already be marked)
      for (i = 0; i < hp->nfields; i++) {
        IRRef dstref = getHeapInfoField(&J->cur, hp, i);

        if (dstref == 0)
          continue;

        if (dstref == ref) {
          DBG_LVL(3, "SCC: Short-cut marking %d as a loop\n",
                  irref_int(ref));
          hp->loop |= UNSINK_LOOP;
        }

        /* DBG_LVL(3, "SCC: .. %d -> %d\n", */
        /*         irref_int(ref), irref_int(dstref)); */
        IRRef dstphi = findPhi(J, dstref);

        DBG_LVL(3, "SCC: .. %d -> %d%s\n",
                irref_int(ref), irref_int(dstref),
                dstphi ? " (phi)" : "");

        dfs2a(J, D, dstphi ? dstphi : dstref);
      }

      if (hp->ind && IR(hp->ind)->o == IR_NEW) {
        // TODO: What happens if we update a NEW node with a PHI node?
        // Can this even happen?
        ref = hp->ind;
      } else
        break;

    } else if (ir->o == IR_PHI) {
      DBG_LVL(3, "SCC: Traversing PHI node %d (%d, %d)\n",
              irref_int(ref), irref_int(ir->op1), irref_int(ir->op2));
      
      // The pre-loop contents of the PHI node, no PHI shadowing
      dfs2a(J, D, ir->op1);  

      // The second argument to a PHI node may be a reference to
      // another PHI node.
      IRRef phi = findPhi(J, ir->op2);
      dfs2a(J, D, phi ? phi : ir->op2);
      break;
    } else {
      LC_ASSERT(0);
      fprintf(stderr, "dfs1a: Unexpected node type: %d\n", ir->o);
      exit(2);
    }
  }

  if (peekStack(&D->p) == ref0) {  // Found SCC root
    popStack(&D->p);

    u1 isloop = (peekStack(&D->s) != ref0) ? 1 : 0;
    IRRef ref2;
    u1 dfs = nodeDfs(J, ref0);
    do {
      ref2 = popStack(&D->s);
      DBG_LVL(3, "SCC: Adding node %d to SCC %d\n",
              irref_int(ref2), irref_int(ref0));

      setNodeScc(J, ref2, dfs);
      if (IR(ref2)->o == IR_NEW) {
        HP(IR(ref2)->op2)->loop |= isloop;
      }
    } while (ref2 != ref0);
  }
}

LC_FASTCALL void
dfs2a(JitState *J, SccData *D, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (ir->o == IR_NEW || ir->o == IR_PHI) {
    if (nodeDfs(J, ref) == 0) {  // Not yet seen.
      dfs1a(J, D, ref);
    } else {
      DBG_LVL(3, "SCC: dfs2: already seen %d\n", irref_int(ref));
      if (nodeScc(J, ref) == 0) {  // SCC not yet found
        u1 ndfs = nodeDfs(J, ref);
        while (!isEmptyStack(&D->p) &&
               nodeDfs(J, peekStack(&D->p)) > ndfs) {
          popStack(&D->p);
        }
      }
    }
  } else {
    DBG_LVL(3, "SCC: dfs2: boring node %d\n", irref_int(ref));
  }
}

// --------------------------------------------------------------

void
markUnsinkable(JitState *J, u2 hi, u1 mode)
{
  HeapInfo *hp = HP(hi);

  if ((hp->loop & UNSINK_VISITED) && (hp->loop & UNSINK_LOOP))
    return;

  hp->loop |= UNSINK_VISITED | mode;
  DBG_LVL(2, "UNSINK: unsink %d, %d\n", hi, mode);

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

// This function is called on each node that is reachable from
// a NEW node which is part of a loop.
LC_FASTCALL void 
markUnsinkable2_aux(JitState *J, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (irt_getmark(ir->t) ||   // already traversed
      !(ir->o == IR_NEW || ir->o == IR_PHI))  // boring node
    return;

  DBG_LVL(3, "SCC: Marking as unsinkable: %d\n", irref_int(ref));
  irt_setmark(ir->t);

  if (ir->o == IR_NEW) {
    int i;
    HeapInfo *hp = HP(ir->op2);

    for (i = 0; i < hp->nfields; i++) {
      IRRef dstref = getHeapInfoField(&J->cur, hp, i);
      if (dstref < REF_BIAS)
        continue;
      IRRef dstphi = findPhi(J, dstref);
      markUnsinkable2_aux(J, dstphi ? dstphi : dstref);
    }
    if (hp->ind)
      markUnsinkable2_aux(J, hp->ind);

  } else {  // ir->o == IR_PHI
    markUnsinkable2_aux(J, ir->op1);
    IRRef phi = findPhi(J, ir->op2);
    markUnsinkable2_aux(J, phi ? phi : ir->op2);
  }
}

// Mark all nodes reachable from cycles as 
void
markUnsinkable2(JitState *J, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (!irt_getmark(ir->t)) {
    if (ir->o == IR_NEW && HP(ir->op2)->loop) {
      // This node is part of a loop.  Traverse it.
      markUnsinkable2_aux(J, ref);
    }
  }
}

void
heapSCCs(JitState *J)
{
  SccData D;
  IRRef ref;
  IRIns *lastir = NULL;

  DBG_LVL(2, "SCC: Finding Heap SCCs and perform allocation sinking\n");

  if (!J->chain[IR_NEW]) // The trivial case, no allocations
    return;

  /* 1. Travers all UPDATEs and set the IND direct fields
     correctly. */
  

  initStack(&D.s, J->cur.nheap);
  initStack(&D.p, J->cur.nheap);
  D.pre = 1;

  // Set PHI node prev fields to zero.  It will (temporarily) be used
  // for marking purposes.  Leaves J->chain[IR_PHI] intact.
  ref = J->chain[IR_PHI];
  while (ref) {
    IRIns *ir = IR(ref);
    ref = ir->prev;
    irt_clearmark(ir->t);  // Used by markUnsinkable
    ir->prev = 0;
  }

  // 1. Find all cycles (including

  // Roots are all NEW instructions after the LOOP marker.
  // Start with last instruction.
  ref = J->chain[IR_NEW];
  while (ref > J->cur.nloop) {
    irt_clearmark(IR(ref)->t);
    dfs2a(J, &D, ref);
    ref = IR(ref)->prev;
  }
  // 1a. Clear mark bit of remaining NEW nodes.
  while (ref) {
    irt_clearmark(IR(ref)->t);
    ref = IR(ref)->prev;
  }

  // 2. Mark all nodes that are stored into external references
  // as unsinkable.
  //
  // Similarly, if we update a PHI node, mark both arguments as unsinkabel.
  for (ref = J->chain[IR_UPDATE]; ref > REF_BIAS; ref = IR(ref)->prev) {
    DBG_LVL(3, "UNSINK: Traversing UPDATE %d\n", irref_int(ref));
    IRIns *ir = IR(ref);
    if (IR(ir->op1)->o != IR_NEW && IR(ir->op2)->o == IR_NEW) {
      HeapInfo *hp = getHeapInfo(J, IR(ir->op2));
      DBG_LVL(3, "UNSINK: Stored into external reference: %d\n",
              irref_int(ref));
      hp->loop |= UNSINK_LOOP;
    } else if (ref > J->cur.nloop) {
      // We might be updating a PHI node.
      IRRef phiref = findPhi(J, ir->op1);
      if (!phiref)
        continue;
      else {
        IRRef ref1 = ir->op1;   // the updated node
        // Mark all involved nodes as unsinkable
        IRRef ref2;
        do {
          LC_ASSERT(phiref != 0);  // Loop invariant

          if (IR(ref1)->o == IR_NEW) {
            HeapInfo *hp = getHeapInfo(J, IR(ref1));
            DBG_LVL(3, "UNSINK: Updated PHI node: %d\n", irref_int(ref1));
            if ((hp->loop & UNSINK_VISITED) == 0) {
              hp->loop |= UNSINK_LOOP | UNSINK_VISITED;
            } else {
              break;  // We found a loop of PHI nodes
            }
          }

          ref2 = IR(phiref)->op2;
          if (IR(ref2)->o == IR_PHI) {
            phiref = findPhi(J, ref2);
            ref1 = ref2;
            continue;
          } else {
            phiref = 0;
            if (IR(ref2)->o == IR_NEW) {
              HeapInfo *hp = getHeapInfo(J, IR(ref2));
              DBG_LVL(3, "UNSINK: Updated PHI node: %d\n", irref_int(ref2));
              hp->loop |= UNSINK_LOOP | UNSINK_VISITED;
            }
          }
        } while (phiref);
      }
    }
  }

  // 3. Mark all nodes reachable from cycles or used in external
  // updates as unsinkable.
  //
  // Starting from the back again, because nodes from the back
  // may reach any other node, but not necessarily vice versa.
  for (ref = J->chain[IR_NEW]; ref; ref = IR(ref)->prev) {
    markUnsinkable2(J, ref);
  }

  destroyStack(&D.s);
  destroyStack(&D.p);

  // Restore prev field of PHI nodes.
  ref = J->chain[IR_PHI];
  if (ref) {
    for (;;) {
      IRIns *ir = IR(ref);
      DBG_LVL(3, "SCC: Relink PHIs %d %p\n", irref_int(ref), lastir);
      if (ir->o == IR_PHI) {
        if (lastir) {
          lastir->prev = ref;
        }
        lastir = ir;
        ref--;
        continue;
      } else if (ir->o == IR_NOP) {  // Possibly deleted PHI node
        ref--;
        continue;
      } else {
        // There must have been at least one PHI node
        LC_ASSERT(lastir != NULL);
        lastir->prev = 0;
        break;
      }
    }
  }

  debugIRDeps(J);
}

void
debugIRDeps(JitState *J)
{
  char buf[100];
  snprintf(buf, 100, "depgraph%d.dot", (int)J->nfragments);
  FILE *f = fopen(buf, "w");
  if (f == NULL)
    return; // Ignore errors
  fprintf(f, "digraph G {\n");
  
  IRRef ref;
  for (ref = J->chain[IR_PHI]; ref >= REF_FIRST; ref = IR(ref)->prev) {
    IRIns *ir = IR(ref);
    fprintf(f, "\tphi%d [label=\"phi%d\"];\n",
            irref_int(ir->op1), irref_int(ir->op1));
    fprintf(f, "\tphi%d -> %d; phi%d -> %d;\n",
	    irref_int(ir->op1), irref_int(ir->op1),
	    irref_int(ir->op1), irref_int(ir->op2));
  }

  
  for (ref = J->chain[IR_NEW]; ref >= REF_FIRST; ref = IR(ref)->prev) {
    fprintf(f, "\t%d [label=\"%d: new\"];\n",
	    irref_int(ref), irref_int(ref));
    HeapInfo *hp = getHeapInfo(J, IR(ref));
    u4 i;
    for (i = 0; i < hp->nfields; i++) {
      IRRef fref = getHeapInfoField(&J->cur, hp, i);
      fprintf(f, "\t%d -> %s%d;\n",
	      irref_int(ref),
	      ref > J->cur.nloop && isLoopVariant(J, fref) ? "phi" : "",
              irref_int(fref));
    }
  }

  for (ref = J->chain[IR_UPDATE]; ref >= REF_FIRST; ref = IR(ref)->prev) {
    IRIns *ir = IR(ref);
    fprintf(f, "\t%s%d -> %s%d [color=red];\n",
	    ref > J->cur.nloop && isLoopVariant(J, ir->op1) ?
            "phi" : "", irref_int(ir->op1),
	    ref > J->cur.nloop && isLoopVariant(J, ir->op2) ?
            "phi" : "", irref_int(ir->op2));
  }

  fprintf(f, "}\n");
  fflush(f);
  fclose(f);
  DBG_PR("Wrote file: %s\n", buf);
}

#undef HP
#undef IR

#endif  /* LC_HAS_JIT */
