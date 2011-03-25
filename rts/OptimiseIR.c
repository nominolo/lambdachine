#include "Common.h"

#if LC_HAS_JIT

#include "IR.h"
#include "Jit.h"
#include "HeapInfo.h"
#include "PrintIR.h"
#include "Bitset.h"
#include "Stats.h"

#include <string.h>

// -- Convenience macros.  Undefined at end of file. -----------------

// Pointer to referenced IR.
#define IR(ref)     (&J->cur.ir[(ref)])
// The instruction currently being optimised
#define foldIns     (&J->fold.ins)

// -------------------------------------------------------------------

// Common subexpression elimination.
//
// WARNING: CSE should not be done for side-effecting operations
// (including allocation).
LC_FASTCALL TRef
optCSE(JitState *J)
{
  IRRef2 op12 = (IRRef2)foldIns->op1 + ((IRRef2)foldIns->op2 << 16);
  IROp op = foldIns->o;
  if (1) {  // TODO: Test whether CSE is enabled
    IRRef ref = J->chain[op];  // first instruction with this opcode
    // There can be no duplicate beyond any argument.
    IRRef lim = foldIns->op2 > foldIns->op1 ? foldIns->op2 : foldIns->op1;

    while (ref > lim) {
      if (IR(ref)->op12 == op12) {
        // Common subexpression found
        DBG_PR("CSE: found duplicate of %d\n", (IRRef1)ref - REF_BIAS);
        return TREF(ref, IR(ref)->t);
      }
      ref = IR(ref)->prev;
    }
  }
  return emitIR(J);
}

// Perform store->load forwarding on the current foldIns.
LC_FASTCALL TRef
optForward(JitState *J)
{
  IROp op = foldIns->o;
  IRRef1 fref = foldIns->op1;
  IRIns *cl_ir = IR(IR(fref)->op1);

  switch (op) {
  case IR_FLOAD:
    {
      // The closure we're referencing has been allocated in this
      // trace.
      if (cl_ir->o == IR_NEW) {
        DBG_PR("ref = %d, new = %d\n", fref - REF_BIAS,
               IR(fref)->op1 - REF_BIAS);
        IRRef1 src = getHeapInfoField(&J->cur, &J->cur.heap[cl_ir->op2],
                                      IR(fref)->op2 - 1);
        LC_ASSERT(src != 0);
        DBG_PR("FWD: Forwarding load: %d (%d, %d)\n",
               src - REF_BIAS, IR(fref)->op1 - REF_BIAS, fref - REF_BIAS);
        return TREF(src, IR(src)->t);
      }

      // TODO: Check for aliases?
      return optCSE(J);
    }
    break;
  default:
    break;
  }
  return emitIR(J);
}


LC_FASTCALL TRef
optFold(JitState *J)
{
  IROp op = foldIns->o;

  // again:
  switch (op) {
  case IR_EQ:
    if (foldIns->op1 == foldIns->op2) {
      DBG_PR("FOLD: trivial guard. %s\n", "EQ");
      return 0;
    }
    break;
  case IR_ILOAD:
    //  iload_again:
    if (irref_islit(foldIns->op1)) {
      IRIns ir = J->cur.ir[foldIns->op1];
      LC_ASSERT(ir.o == IR_KWORD);
      Closure *c = (Closure*)J->cur.kwords[ir.u];
      DBG_PR("FOLD: ILOAD for static closure\n%s", "");
      return emitKWord(J, (Word)getFInfo(c), LIT_INFO);
    } else {
      IRIns *left = IR(foldIns->op1);
      if (left->o == IR_NEW) {
        DBG_PR("FOLD: ILOAD for NEW closure\n%s", "");
        return left->op1;
      }
    }
    break;
  case IR_SLOAD:
    if (J->slot[foldIns->op1])
      return J->slot[foldIns->op1];
    break;
  case IR_NEW:
    // TODO: Allocating a loop-invariant value only needs to be done
    // once.
    break;
  default:
    ;
  }

  if (ir_mode[op] & IRM_A) {
    // Don't optimise allocation, yet.
    return emitIR(J);
  } else if (ir_mode[op] & IRM_L && op != IR_ILOAD) {
    return optForward(J);
  } else {
    //printf("CSE on %s\n", ir_name[op]);
    return optCSE(J);
  }
}

LC_FASTCALL void
optUnrollLoop(JitState *J)
{
  TRef *renaming;
  u4 max_renamings = J->cur.nloop - REF_BIAS;
  IRRef ref;
  u4 nextsnap = 0;

  DBG_LVL(2,"max_renamings = %u\n", max_renamings);
  renaming = xmalloc(max_renamings * sizeof(*renaming));
  renaming -= REF_BIAS;

  for (ref = REF_FIRST; ref < J->cur.nloop; ref++) {
    IRIns *ir = IR(ref);
    IRRef1 op1, op2;

# define RENAME(r) \
    (((r) < ref && (r) > REF_BIAS) ? \
      tref_ref(renaming[(r)]) : (r))

    DBG_LVL(2, "UNROLL: %s", "");
    IF_DBG_LVL(2, printIR(&J->cur, *ir));

    // If there is a snapshot at the current instruction we need to
    // "replay" it first.  Simply loop over the elements in the
    // snapshot and apply the substitution and modify the slot table.
    if (nextsnap < J->cur.nsnap && J->cur.snap[nextsnap].ref == ref) {
      SnapShot *snap = &J->cur.snap[nextsnap];
      SnapEntry *p = J->cur.snapmap + snap->mapofs;
      int i;
      for (i = 0; i < snap->nent; i++, p++) {
        IRRef1 r = RENAME(snap_ref(*p));
        IRIns *ir = IR(r);
        int slot = snap_slot(*p);
        //printf("setting slot %d, to %d, %d\n", slot, r - REF_BIAS, ir->t);
        J->slot[slot] = TREF(r, ir->t);
      }
      J->pc = J->cur.startpc + (ptrdiff_t)(i4)(*p);
      // base slot is updated separately via ENTER and RET instructions.
      nextsnap++;
    }

    // Apply renaming to operands if needed.
    //
    // If the operand is a reference and we already have a valid
    // renaming, then use that.  Otherwise, it remains unchanged.
    op1 = (irm_op1(ir_mode[ir->o]) == IRMref) ? RENAME(ir->op1) : ir->op1;
    op2 = (irm_op2(ir_mode[ir->o]) == IRMref) ? RENAME(ir->op2) : ir->op2;

    //printf("op1 = %d, op2 = %d\n", op1 - REF_BIAS, op2 - REF_BIAS);
    renaming[ref] = emit(J, IRT(ir->o, ir->t), op1, op2);

    // Some instructions require special treatment, because they affect
    // state outside of the IR buffer.
    switch (ir->o) {

      // FRAME and RET instructions keep track of the boundaries of
      // the stack
    case IR_FRAME:
      J->baseslot += ir->op1;
      J->base = J->slot + J->baseslot;
      J->maxslot = ir->op2;
      break;
    case IR_RET:
      J->baseslot -= ir->op1;
      J->base = J->slot + J->baseslot;
      J->maxslot = ir->op1 - 3;
      break;

      // For NEW instructions we need to adjust the HeapInfo
    case IR_NEW:
      {
        HeapInfo *hpold = &J->cur.heap[ir->op2];
        u2 entry = cloneHeapInfo(J, tref_ref(renaming[ref]), ir->op2);
        HeapInfo *hpnew = &J->cur.heap[entry];
        IRIns *ir2 = IR(tref_ref(renaming[ref]));
        u2 i;
        for (i = 0; i < hpnew->nfields; i++) {
          IRRef r = getHeapInfoField(&J->cur, hpold, i);
          setHeapInfoField(&J->cur, hpnew, i, RENAME(r));
        }
        ir2->op2 = entry;
      }
      break;
    default: break;
    }

    DBG_LVL(2, "   %d => ", ref - REF_BIAS);
    IF_DBG_LVL(2, printIRRef(&J->cur, tref_ref(renaming[ref])));
    DBG_LVL(2, "\n%s", "");
# undef RENAME
  }

  // Emit PHI instructions
  //
  // NOTE: That some PHI instructions may not be needed.  We eliminate
  // those in [optDeadCodeElim].
  for (ref = REF_FIRST; ref < J->cur.nloop; ref++) {
    TRef tr = renaming[ref];
    IRIns *ir = IR(tref_ref(tr));
    // We need PHI nodes for all instructions that:
    //   - have been redefined in the unrolled loop
    //   - return a result
    //   - ar not FREFS
    // For both IRs involved we set the IRT_PHI flag to efficiently
    // detect wether any node is argument to a PHI node.
    if (tref_t(tr) != IRT_VOID &&
        tref_ref(tr) > J->cur.nloop &&
        ir->o != IR_FREF) {
      irt_setphi(IR(ref)->t);
      irt_setphi(ir->t);
      emit(J, IRT(IR_PHI, ir->t), ref, tref_ref(tr));
    }
  }

  xfree(renaming + REF_BIAS);
}

// Find the corresponding twin of a referenced involved in a PHI node.
//
// For example:
//
//     t1  ADD a b
//     --- LOOP ---
//     x1  SUB t1 c  ; reference to t1 or t2
//     t2  ADD t1 b
//     x2  SUB t2 d  ; reference to just t2
//     -   PHI t1 t2
//
// The basic principle is that all PHI nodes semantically occur right
// after the LOOP marker.  The reference to `t1` in `x1` therefore refers
// to `t1` in the first iteration and thereafter to `t2` from the
// previous iteration.
//
LC_FASTCALL IRRef
findPhiTwin(JitState *J, IRRef ref)
{
  if (ref < J->cur.nloop && irt_getphi(IR(ref)->t)) {
    // We have a reference to a loop variant variable
    IRRef1 r = J->chain[IR_PHI];
    while (r) {
      IRIns *ir = IR(r);
      if (ir->op1 == ref)
	return ir->op2;
      r = ir->prev;
    }
    // We must have a matching PHI node if the IRT_PHI flag is set.
    // So we should never reach this point.
    fprintf(stderr, "Could not find PHI twin for: %d\n",
            ref - REF_BIAS);
    LC_ASSERT(0);
    return 0;
  } else
    return 0;
}

// Mark reference [ref] from reference site [site].
INLINE_HEADER
void
markIRRef(JitState *J, IRRef ref, IRRef site)
{
  if (irref_islit(ref))
    return;

  IRIns *ir = IR(ref);

  // If we are marking a node involved in a PHI node, mark the
  // PHI node as well.  However, we only do this the first time
  // around, because marking a PHI node uses linear search.
  if (site > J->cur.nloop && irt_getphi(ir->t) && !irt_getmark(ir->t)) {
    IRRef phiref = tref_ref(J->chain[IR_PHI]);
    while (phiref) {
      if (IR(phiref)->op1 == ref) {
        //DBG_PR("Setting mark for PHI: %d\n", phiref - REF_BIAS);
        irt_setmark(IR(phiref)->t);
        break;
      }
      phiref = IR(phiref)->prev;
    }
  }
  //DBG_PR("Setting mark for: %d from %d\n", ref - REF_BIAS,
  //       site - REF_BIAS);
  irt_setmark(ir->t);
}

// Dead code elimination.
//
// Marks all live-out references and their dependencies.  Anything not
// marked at the end is dead code and can be deleted (replaced by NOPs).
//
// Allocation sinking is done in a separate pass.

INLINE_HEADER void markSnapshot(JitState *J, SnapShot *snap);
INLINE_HEADER void markIRIns(JitState *J, IRRef ref);
//
LC_FASTCALL void
optDeadCodeElim(JitState *J)
{
  // Marking works in two steps.
  //
  // a. Mark all variables mentioned in snapshots.
  //
  // We start from the end so that if a variable `x` is marked and `x`
  // is involved in a PHI node, then marked `IR(x)` implies marked
  // `PHI x`.
  //
  // b. Mark all instructions reachable from those marked above.
  //
  // Again we start from the back, because data dependencies point
  // backwards.

  // We mark the unrolled loop and the loop intro separately.  This is
  // needed to deal effectively with PHI references.  See comments in
  // [markIRIns].
  int snapidx = J->cur.nsnap - 1;
  IRRef ref;

  // Mark unrolled loop
  for ( ; snapidx >= 0 && J->cur.snap[snapidx].ref > J->cur.nloop;
       snapidx--)
    markSnapshot(J, &J->cur.snap[snapidx]);

  for (ref = J->cur.nins - 1; ref > J->cur.nloop; ref--)
    markIRIns(J, ref);

  // Mark loop header
  for ( ; snapidx >= 0; snapidx--)
    markSnapshot(J, &J->cur.snap[snapidx]);

  for (ref = J->cur.nloop - 1; ref >= REF_FIRST; ref--)
    markIRIns(J, ref);

  // 4. Replace all unmarked instructions by NOPs
  //
  // We also need to fix up the previous pointer and [J->chain.]
  memset(J->chain, 0, sizeof(J->chain));

  for (ref = REF_FIRST; ref < J->cur.nins; ref++) {
    IRIns *ir = IR(ref);
    if (!irt_getmark(ir->t) && ir->o != IR_LOOP) {
      // Remove PHI tag from arguments if we're deleting a PHI node.
      if (ir->o == IR_PHI) {
	printf("Deleting PHI for %d, %d\n", ir->op1 - REF_BIAS, ir->op2 - REF_BIAS);
        irt_clearphi(IR(ir->op1)->t);
        irt_clearphi(IR(ir->op2)->t);
      }
      ir->o = IR_NOP;
      ir->t = IRT_VOID;
      ir->prev = J->chain[IR_NOP];
      J->chain[IR_NOP] = ref;
    } else {
      ir->prev = J->chain[ir->o];
      J->chain[ir->o] = ref;
    }
    irt_clearmark(ir->t);
  }
}

INLINE_HEADER void markSnapshot(JitState *J, SnapShot *snap)
{
  int j;
  SnapEntry *se = &J->cur.snapmap[snap->mapofs];
  for (j = 0; j < snap->nent; j++, se++) {
    markIRRef(J, snap_ref(*se), snap->ref);
  }
}

INLINE_HEADER void markIRIns(JitState *J, IRRef ref)
{
  int i;
  IRIns *ir = IR(ref);
  if (ir_mode[ir->o] & (IRM_S|IRM_G))
    irt_setmark(ir->t);
  if (irt_getmark(ir->t)) {
    if (irm_op1(ir_mode[ir->o]) == IRMref)
      markIRRef(J, ir->op1, ref);
    if (irm_op2(ir_mode[ir->o]) == IRMref)
      markIRRef(J, ir->op2, ref);
    if (ir->o == IR_NEW) {
      HeapInfo *h = &J->cur.heap[ir->op2];
      for (i = 0; i < h->nfields; i++) {
        markIRRef(J, getHeapInfoField(&J->cur, h, i), ref);
      }
    }
  }
}

// Dead assignment and update elimination.
//
// This uses liveness information to remove stores to otherwise dead
// variables.
//
// TODO: This could potentially be integrated with register
// allocation, since both need liveness information.
//
LC_FASTCALL void
optDeadAssignElim(JitState *J)
{
  // Handling of PHI nodes:
  //
  // Variables mentioned on the *rhs* of PHI variables are live at the
  // end of the loop.  Variables mentioned on the *lhs*, are live
  // before the loop body starts.

  int i;
  u4 bsize = J->cur.nins - REF_BIAS;
  Bitset lives[BITSET_SIZE(bsize)];
  IRRef ref;
  IRRef nextsnap = 0;
  SnapShot *snap = NULL;
  clearBitset(lives, bsize);
  //printBitset(lives, bsize);

  if (J->cur.nloop && J->chain[IR_UPDATE]) {
    IRIns *ir;
    ref = J->cur.nins - 1;
    ir = IR(ref);

    // Initialise with PHIs
    while (ir->o == IR_NOP || ir->o == IR_PHI) {
      if (ir->o == IR_PHI)
        setBit(lives, ir->op2 - REF_BIAS);
      ref--;
      ir = IR(ref);
    }

    if (J->cur.nsnap > 0) {
      snap = &J->cur.snap[J->cur.nsnap - 1];
      nextsnap = snap->ref;
    }

    IRRef prev_upd = J->chain[IR_UPDATE];
    for ( ; ref > J->cur.nloop; ref--) {
      //printf("%d: ", ref - REF_BIAS);
      //printBitset(lives, bsize);
      ir = IR(ref);
      if (ir->o == IR_UPDATE) {
        printf("Found UPDATE: \n");
        printIR(&J->cur, *ir);
        if (!irref_islit(ir->op1) && !getBit(lives, ir->op1 - REF_BIAS)) {
          ir->o = IR_NOP;
          // Update prev pointer.
          if (ref == prev_upd)
            prev_upd = ir->prev;
          else
            IR(prev_upd)->prev = ir->prev;
          continue;
        } else {
          prev_upd = ref;
        }
      }
      // Update lives
      if (irt_type(ir->t) != IRT_VOID)
        clearBit(lives, ref - REF_BIAS);
      if (irm_op1(ir_mode[ir->o]) == IRMref && !irref_islit(ir->op1))
        setBit(lives, ir->op1 - REF_BIAS);
      if (irm_op2(ir_mode[ir->o]) == IRMref && !irref_islit(ir->op2))
        setBit(lives, ir->op2 - REF_BIAS);

      if (ref == nextsnap) {
        SnapEntry *se = J->cur.snapmap + snap->mapofs;
        for (i = 0; i < snap->nent; i++, se++) {
          if (!irref_islit(snap_ref(*se)))
            setBit(lives, snap_ref(*se) - REF_BIAS);
        }
      }
    }
  }
}



#endif
