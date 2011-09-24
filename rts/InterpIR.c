#include "Jit.h"
#include "IR.h"
#include "Capability.h"
#include "Thread.h"
#include "PrintIR.h"
#include "MiscClosures.h"
#include "HeapInfo.h"
#include "StorageManager.h"
#include "Stats.h"

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
         base, pc, pcmax, (int)(pcmax - pc), pcloop, (int)(pcloop - pc));

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
  if (LC_UNLIKELY(pc >= pcmax)) { pc = pcloop; pcref = F->nloop + 1; } \
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

 op_NEW:
  if (!ir_issunken(pc)) {
    // do actual allocation on trace
    HeapInfo *hp = &F->heap[pc->op2];
    int j;
    recordEvent(EV_ALLOC, hp->nfields + 1);
    Closure *cl = allocClosure(wordsof(ClosureHeader) + hp->nfields);
    setInfo(cl, (InfoTable*)vals[pc->op1]);
    for (j = 0; j < hp->nfields; j++) {
      cl->payload[j] = vals[getHeapInfoField(F, hp, j)];
    }
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
    for (i = 0; i < F->nsnap; i++) {
      if (F->snap[i].ref == pcref) {
        snap = &F->snap[i];
        break;
      }
    }
    LC_ASSERT(snap != 0);
    snap->count++;
    se = F->snapmap + snap->mapofs;
    DBG_PR("Snapshot: %d, Snap entries: %d, slots = %d\n",
           i, snap->nent, snap->nslots);
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

  hp = &F->heap[ir->op2];
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

  for (j = 0; j < hp->nfields; j++)
    cl->payload[j] = restoreValue(F, vals, getHeapInfoField(F, hp, j));

  return (Word)cl;
}

#undef IR
