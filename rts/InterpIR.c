#include "Jit.h"
#include "IR.h"
#include "Capability.h"
#include "Thread.h"
#include "PrintIR.h"
#include "MiscClosures.h"
#include "HeapInfo.h"
#include "StorageManager.h"
#include "Stats.h"
#include "PrintClosure.h"

/**

The function `irEngine` implements an interpreter for the trace IR.
While this is obviously very slow, it can be useful to collect
execution statistics and to serve as a reference semantics for the
generated machine code.
  
*********************************************************************/

typedef void* Inst;

#define IR(ref)     (&F->ir[(ref)])

Word restoreValue(Fragment *F, Word *vals, IRRef ref);


int
irEngine(Capability *cap, Fragment *F)
{
  static Inst disp[] = {
#define IRIMPL(name, f, o1, o2)  &&op_##name,
    IRDEF(IRIMPL)
#undef IRIMPL
    &&stop
  };

  
  IRRef ref;
  Thread *T = cap->T;
  Word nphis = F->nphis;
  Word *base = T->base - 1;
  Word szins = F->nins - F->nk;
  Word vals_[szins + nphis];
  Word *phibuf = &vals_[szins]; /* For parallel copy of PHI nodes */
  Word *vals = vals_ - (int)F->nk;
  Word *hp = G_storage.hp;
  Word *hplim = G_storage.limit;
  int heapcheck_failed = 0;
  IRIns *pc = F->ir + REF_FIRST;
  IRRef pcref = REF_FIRST;
  IRIns *pcmax = F->ir + F->nins;
  IRIns *pcloop = F->nloop ? F->ir + F->nloop + 1 : pc;
  //int count = 100;

  DBG_PR("*** Executing trace.\n"
         "***   base  = %p\n"
         "***   pc    = %p\n"
         "***   pcmax = %p (%d)\n"
         "***   loop  = %p (%d)\n",
         T->base, pc, pcmax, (int)(pcmax - pc), pcloop, (int)(pcloop - pc));

  for (ref = F->nk; ref < REF_BIAS; ref++) {
    switch (IR(ref)->o) {
    case IR_KINT:   vals[ref] = (Word)IR(ref)->i; break;
    case IR_KBASEO: vals[ref] = (Word)(T->base + IR(ref)->i); break;
    case IR_KWORD:  vals[ref] = (Word)(F->kwords[IR(ref)->u]); break;
    default:
      LC_ASSERT(0); break;
    }
    DBG_LVL(2, "%d, %" FMT_WordX "\n", ref - REF_BIAS, vals[ref]);
  }
  vals[REF_BASE] = (Word)base;

  goto *disp[pc->o];

# define DISPATCH_NEXT \
  if (irt_type(pc->t) != IRT_VOID && pc->o != IR_PHI) { \
    if (irt_type(pc->t) == IRT_I32) \
      DBG_LVL(2, "         ===> %" FMT_Int "\n", vals[pcref]); \
    else \
      DBG_LVL(2, "         ===> 0x%" FMT_WordX "\n", vals[pcref]); } \
  ++pc; ++pcref; \
  if (LC_UNLIKELY(pc >= pcmax)) { pc = pcloop; pcref = F->nloop ? F->nloop + 1 : REF_FIRST; } \
  if (pc->o != IR_NOP) { \
    DBG_LVL(2, "[%d] ", pcref - REF_BIAS); \
    IF_DBG_LVL(2, printIR(F, *pc)); } \
  goto *disp[pc->o]

 op_NOP:
 op_FRAME:
 op_RET:
 op_LOOP:
  DISPATCH_NEXT;

 op_PHI:
  {
    /* PHI nodes represent parallel assignments, so as soon as we
       discover the first PHI node, we perform all assignments in
       parallel. */
    LC_ASSERT(nphis > 0);
    u2 i;
    DBG_LVL(3, "             ( ");
    for (i = 0; i < nphis; i++) {
      DBG_LVL(3, "%d ", irref_int(pc[i].op2));
      phibuf[i] = vals[pc[i].op2];
    }
    DBG_LVL(3, ") --> ( ");
    for (i = 0; i < nphis; i++) {
      DBG_LVL(3, "%d ", irref_int(pc[i].op1));
      vals[pc[i].op1] = phibuf[i];
    }
    DBG_LVL(3, ")  [%d phis]\n", (int)nphis);
    pc += nphis - 1;
    //vals[pc->op1] = vals[pc->op2];
    DISPATCH_NEXT;
  }

 op_LT:
  recordEvent(EV_CMP, 0);
  if (!((WordInt)vals[pc->op1] < (WordInt)vals[pc->op2]))
    goto guard_failed;
  DISPATCH_NEXT;

 op_GE:
  recordEvent(EV_CMP, 0);
  if (!((WordInt)vals[pc->op1] >= (WordInt)vals[pc->op2]))
    goto guard_failed;
  DISPATCH_NEXT;

 op_LE:
  recordEvent(EV_CMP, 0);
  if (!((WordInt)vals[pc->op1] <= (WordInt)vals[pc->op2]))
    goto guard_failed;
  DISPATCH_NEXT;

 op_GT:
  recordEvent(EV_CMP, 0);
  if (!((WordInt)vals[pc->op1] > (WordInt)vals[pc->op2]))
    goto guard_failed;
  DISPATCH_NEXT;

 op_EQ:
  recordEvent(EV_CMP, 0);
  if (!((WordInt)vals[pc->op1] == (WordInt)vals[pc->op2])) {
    goto guard_failed;
  }
  DISPATCH_NEXT;

 op_NE:
  recordEvent(EV_CMP, 0);
  if (!((WordInt)vals[pc->op1] != (WordInt)vals[pc->op2]))
    goto guard_failed;
  DISPATCH_NEXT;

 op_ADD:
  recordEvent(EV_ALU, 0);
  vals[pcref] = vals[pc->op1] + vals[pc->op2];
  DISPATCH_NEXT;

 op_SUB:
  recordEvent(EV_ALU, 0);
  vals[pcref] = vals[pc->op1] - vals[pc->op2];
  DISPATCH_NEXT;

 op_MUL:
  recordEvent(EV_MUL, 0);
  vals[pcref] = (WordInt)vals[pc->op1] * (WordInt)vals[pc->op2];
  DISPATCH_NEXT;

 op_DIV:
  recordEvent(EV_REMDIV, 0);
  if (LC_LIKELY(vals[pc->op2] != 0))
    vals[pcref] = (WordInt)vals[pc->op1] / (WordInt)vals[pc->op2];
  else
    LC_ASSERT(0);
  DISPATCH_NEXT;

 op_REM:
  recordEvent(EV_REMDIV, 0);
  if (LC_LIKELY(vals[pc->op2] != 0))
    vals[pcref] = (WordInt)vals[pc->op1] % (WordInt)vals[pc->op2];
  else
    LC_ASSERT(0);
  DISPATCH_NEXT;

 op_FREF:
  vals[pcref] = (Word)(((Closure*)vals[pc->op1])->payload + (pc->op2 - 1));
  DISPATCH_NEXT;

 op_FLOAD:
  recordEvent(EV_LOAD, 0);
  vals[pcref] = *((Word*)vals[pc->op1]);
  DISPATCH_NEXT;

 op_SLOAD:
  recordEvent(EV_LOAD, 0);
  vals[pcref] = base[pc->op1];
  DISPATCH_NEXT;

 op_ILOAD:
  recordEvent(EV_LOAD, 0);
  vals[pcref] = (Word)getInfo(vals[pc->op1]);
  DISPATCH_NEXT;

 op_HEAPCHK:
  //DISPATCH_NEXT;
  if (LC_LIKELY(hp + pc->u <= hplim)) {
    DBG_LVL(2, "               Hp: %p => %p\n", hp, hp + pc->u);
    hp += pc->u;
    DISPATCH_NEXT;
  } else {
    StorageManagerState *M = &G_storage;
    M->hp = hp;
    M->limit = hplim;
    markCurrentBlockFull(M);

    if (M->nfull < M->nextgc) {
      /* We just reached the end of the current block, no full GC
         necessary, yet.  Just grab a new block and re-enter trace.
         
         This is safe even in compiled code provided that:
         
           - the heap pointer is a dedicated register (or stack slot)

           - the heap limit is in a dedicated register or stack slot
             (more likely the latter)
      */
      
      makeCurrent(M, getEmptyBlock(M));
      hp = M->hp;
      hplim = M->limit;
      goto op_HEAPCHK;          /* Retry */

      /* NOTE: We cannot get into an infinite loop here because on
         each but the last iteration we increment M->nfull */

    } else {
      
      /* GC necessary.  It's easiest to just force execution back to
         the interpreter.  Eventually, the interpreter will try to
         allocate an object, fail the heap check, and trigger a
         garbage collection.

         NOTE: Just exiting the trace may trigger allocation due to
         sunken allocations.  This means, we could run out of memory
         while allocating an object from the heap snapshot.  To avoid
         this, we temporarily suppress GC until after the snapshot has
         been restored.  We then set `G_storage.limit = G_storage.hp`
         which will trigger garbage at the next allocation
         instruction executed by the interpreter.

         There is one degenerate case here.  Consider these events:
         
           1. We exit a trace

           2. The interpreter continues execution but doesn't perform
              any allocations.

           3. The interpreter finds another trace entrance 
         
      */
      makeCurrent(M, getEmptyBlock(M));
      heapcheck_failed = 1;
      goto guard_failed;
      /* fprintf(stderr, "Exiting due to failed on-trace heap check.\n"); */
      /* exit(123);  */
    }
  }
  fprintf(stderr, "FATAL: unreachable in op_HEAPCHK\n");
  exit(13);

 op_NEW:
  if (!ir_issunken(pc)) {
    // do actual allocation on trace
    HeapInfo *hpi = &F->heap[pc->op2];
    int j;
    recordEvent(EV_ALLOC, hpi->nfields + 1);
    LC_ASSERT(hpi->hp_offs < 0);
    Closure *cl = (Closure*)(hp + (hpi->hp_offs - 1));
    //Closure *cl = allocClosure(wordsof(ClosureHeader) + hpi->nfields);
    setInfo(cl, (InfoTable*)vals[pc->op1]);
    for (j = 0; j < hpi->nfields; j++) {
      DBG_LVL(3, "    field %d: %d, %" FMT_WordX "\n", j,
	      irref_int(getHeapInfoField(F, hpi, j)),
	      vals[getHeapInfoField(F, hpi, j)]);
      cl->payload[j] = vals[getHeapInfoField(F, hpi, j)];
    }
    /* DBG_LVL(3, "Hp = %p size=%d offs=%d Clos=%p\n", */
    /*         hp, hpi->nfields, hpi->hp_offs, cl); */
    vals[pcref] = (Word)cl;
  } else {
    vals[pcref] = 0;  // to trigger an error if accessed
  }
  DISPATCH_NEXT;

 op_UPDATE:
  {
    recordEvent(EV_UPDATE, 0);
    Closure *oldnode = (Closure *)vals[pc->op1];
    Closure *newnode = (Closure *)base[pc->op2];
    setInfo(oldnode, (InfoTable*)&stg_IND_info);
    oldnode->payload[0] = (Word)newnode;
    DISPATCH_NEXT;
  }

 op_SAVE:
  /*

     SAVE <snap_id:16>

     Write all values mentioned in the snapshot to the stack and
     adjust the BASE pointer to the value specified in the snapshot.

     This is used to implement traces which increase the stack during
     execution.

  */
  {
    //static int countdown = 10;
    u4 snapid = pc->op1;
    SnapShot *snap = &F->snap[snapid];
    SnapEntry *p = &F->snapmap[snap->mapofs];
    u4 nent = snap->nent;
    u4 nslots = snap->nslots;
    u4 baseslot = (u4)p[nent + 1];
    Word *realbase = base + 1;
    int i;
    for (i = 0; i < nent; i++, p++) {
      //doWeNeedToChangeTheTypeOf_snap_slot_fromByteToWord16
      int reg = (int)snap_slot(*p) - 1;
      IRRef ref = snap_ref(*p);
      DBG_LVL(2, "Storing %" FMT_WordX " into base[%d]\n",
	      vals[ref], reg);
      if (IR(ref)->o == IR_KBASEO) {
	base[reg + 1] = realbase + IR(ref)->i;
      } else {
	base[reg + 1] = vals[ref];
      }
    }

    if (stackOverflow(T, T->top, nslots))
      sayonara("Stack Overflow");

    Word *old_base = base;
    T->top = base + 1 + nslots;
    base += baseslot - 1;
    T->base = base + 1;
    //    vals[REF_BASE] = T->base;
    //    sayonara("Need to treat KBASEO differently!");

    DBG_LVL(2, "base goes from %p to %p (delta: %d), top = %p (%d)\n",
	    old_base + 1, base + 1, baseslot - 1, T->top, nslots );
    IF_DBG_LVL(2, printFrame(stderr, old_base + 1, T->top));
    //if (--countdown == 0) sayonara("Testing");
  }
  DISPATCH_NEXT;

 op_RLOAD:
 op_FSTORE:
 op_RENAME:


 op_BNOT: op_BAND: op_BOR: op_BXOR:
 op_BSHL: op_BSHR: op_BSAR:
 op_BROL: op_BROR:

  // These should never be executed.
 op_BASE:
 op_KINT:
 op_KWORD:
 op_KBASEO:
  LC_ASSERT(0);

 guard_failed:
  DBG_PR("Exiting at %d\n", pcref - REF_BIAS);

  {
    int i;
    SnapShot *snap = 0;
    SnapEntry *se;
    /* Reconstructing from the snapshot may cause allocation
       which in turn may cause garbage collection.  We temporarily
       suppress GC to avoid having to attach pointer info
       to each snapshot. */
    G_storage.gc_inhibited = 1;
    G_storage.hp = hp;
    G_storage.limit = hplim;
    for (i = 0; i < F->nsnap; i++) {
      if (F->snap[i].ref == pcref) {
        snap = &F->snap[i];
        break;
      }
    }
    LC_ASSERT(snap != 0);
    int snap_id = i;
    snap->count++;
    se = F->snapmap + snap->mapofs;
    DBG_PR("Snapshot: %d, Snap entries: %d, slots = %d\n",
           snap_id, snap->nent, snap->nslots);
    recordEvent(EV_EXIT, snap->nent);
    for (i = 0; i < snap->nent; i++, se++) {
      BCReg s = snap_slot(*se);
      IRRef r = snap_ref(*se);

      DBG_PR("base[%d] = ", s - 1);
      base[s] = restoreValue(F, vals, r);

      IF_DBG_LVL(1, printSlot(stderr, base + s); fprintf(stderr, "\n"));
      //DBG_PR("0x%" FMT_WordX "\n", base[s]);
    }
    DBG_PR("Base slot: %d\n", se[1]);
    //    se[1] = 
    T->pc = (BCIns *)F->startpc + (int)se[0];
    T->base = base + se[1];
    T->top = base + snap->nslots;

    if (heapcheck_failed) {
      /* Force a GC next time an allocation is triggered. */
      G_storage.limit = G_storage.hp;
      heapcheck_failed = 0;
      // DBG_PR("GC no longer inhibited\n");
    }
    G_storage.gc_inhibited = 0;

    if (G_jitstep & STEP_EXIT_TRACE) {
      fprintf(stderr, "Exited trace: at exit %d", snap_id);
      getchar();
    }

    //printFrame(T->base, T->top);
    return 0;
  }

 stop:
  return 1;
}

Word
restoreValue(Fragment *F, Word *vals, IRRef ref)
{
  IRIns *ir = IR(ref);
  HeapInfo *hp;
  Closure *cl;
  int j;

  // We only need to treat allocations specially.
  if (ir->o != IR_NEW)
    return vals[ref];

  hp = getHeapInfo(F, ir);
  // Store has *not* been sunken, i.e., allocation occurred on-trace
  if (!ir_issunken(ir))
    return vals[ref];

  // Otherwise we need to do the allocation now, possibly recursively
  //
  // TODO: Can we have mutually recursive sunken refs?
  recordEvent(EV_ALLOC, 1 + hp->nfields);
  cl = allocClosure(wordsof(ClosureHeader) + hp->nfields);
  setInfo(cl, (InfoTable*)vals[ir->op1]);
  DBG_PR("(alloc[%lu])", wordsof(ClosureHeader) + hp->nfields);

  for (j = 0; j < hp->nfields; j++) {
    DBG_LVL(3, "{%d:%d}", j, irref_int(getHeapInfoField(F, hp, j)));
    cl->payload[j] = restoreValue(F, vals, getHeapInfoField(F, hp, j));
  }

  return (Word)cl;
}

#undef IR
