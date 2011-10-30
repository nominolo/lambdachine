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

// There we go.

// -------------------------------------------------------------------

INLINE_HEADER IRIns *
followAbstractINDs(JitState *J, IRIns *ir)
{
  while (ir->o == IR_NEW) {
    HeapInfo *hp = getHeapInfo(&J->cur, ir);
    if (hp->ind)
      ir = IR(hp->ind);
    else
      break;
  }

  return ir;
}


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
      IRIns *left = followAbstractINDs(J, IR(foldIns->op1));
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
  case IR_HEAPCHK:
    {
      /* Fold multiple heap checks into one, but not across the loop
         boundary */
      IRRef ref = J->chain[IR_HEAPCHK];
      if ( ref &&
           ((J->cur.nloop && ref > J->cur.nloop) ||
            (!J->cur.nloop && ref) )) {
        DBG_PR("FOLD: HEAPCHECK += %u\n", foldIns->u);
        IRIns *ir = IR(ref);
        ir->u += foldIns->u;
        return ref;
      } else {
        DBG_PR("FOLD: new HEAPCHECK %u\n", foldIns->u);
        return emitIR(J);
      }
    }
    break;
  case IR_NEW:
    // TODO: Allocating a loop-invariant value only needs to be done
    // once.
    break;
  case IR_UPDATE:
    {
      // If the updated node has been allocated on-trace, just update
      // its `ind` field.
      IRIns *left = IR(foldIns->op1);
      if (left->o == IR_NEW) {
        HeapInfo *hp = getHeapInfo(&J->cur, left);
        LC_ASSERT(!hp->ind && foldIns->op2);
        hp->ind = foldIns->op2;
        return emitIR(J);
      }
    }
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
    //   - return a result, and
    //   - are live-out or needed to compute another live-out value, and
    //   - whose renaming is different from the original
    //   - are not FREFS
    //
    // For both IRs involved we set the IRT_PHI flag to efficiently
    // detect wether a node is argument to a PHI node.
    //
    if (tref_t(tr) != IRT_VOID &&
        tref_ref(tr) >= REF_FIRST &&
        tref_ref(tr) != ref &&
        //tref_ref(tr) > J->cur.nloop &&
        ir->o != IR_FREF) {
      DBG_LVL(3, "UNROLL: Adding PHI for %d/%d",
              irref_int(ref), irref_int(tref_ref(tr)));
      irt_setphi(IR(ref)->t);
      //irt_setphi(ir->t);
      emit(J, IRT(IR_PHI, ir->t), ref, tref_ref(tr));
    }
  }

  IF_DBG_LVL(3,
             {
               IRRef ref;
               DBG_PR("UNROLL: Renaming: \n");
               for (ref = REF_FIRST; ref < J->cur.nloop; ref++) {
                 DBG_PR("         %d -> %d\n", irref_int(ref),
                        irref_int(tref_ref(renaming[ref])));
               }
             });

  xfree(renaming + REF_BIAS);
}

// Slow-path of [findPhi].
LC_FASTCALL IRRef
findPhi_aux(JitState *J, IRRef ref)
{
  LC_ASSERT(ref < J->cur.nloop && irt_getphi(IR(ref)->t));
  //  DBG_LVL(3, "findPhi: %d\n", irref_int(ref));
  
  // Linear search through the PHI nodes.  We expect there to be not
  // very many PHI nodes in general and they are in adjacent cache
  // lines so binary search probably wouldn't be (much) faster.
  //
  // We don't use the prev field here because during the SCC phase of
  // allocation sinking it will be undefined.
  IRRef r = J->chain[IR_PHI];
  while (r) {
    IRIns *ir = IR(r);
    if (ir->o == IR_PHI && ir->op1 == ref)
      return r;
    if (!(ir->o == IR_NOP || ir->o == IR_PHI))
      break;
    r--;
  }
  
  fprintf(stderr, "findPhi_aux: Could not find PHI noder for: %d",
          irref_int(ref));
  exit(2);
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
        /* DBG_LVL(3, "DCE: Setting mark for PHI(%d,%d): %d\n", */
        /*         irref_int(IR(phiref)->op1), */
        /*         irref_int(IR(phiref)->op2), */
        /*         irref_int(phiref)); */
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

INLINE_HEADER void markSnapshot(JitState *J, SnapShot *snap, bool post_sink);
INLINE_HEADER void markIRIns(JitState *J, IRRef ref, bool post_sink);
//
LC_FASTCALL void
optDeadCodeElim(JitState *J, bool post_sink)
{
  IRRef ref;

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

  // Mark unrolled loop
  for ( ; snapidx >= 0 && J->cur.snap[snapidx].ref > J->cur.nloop;
       snapidx--)
    markSnapshot(J, &J->cur.snap[snapidx], post_sink);

  for (ref = J->cur.nins - 1; ref > J->cur.nloop; ref--)
    markIRIns(J, ref, post_sink);

  // Mark loop header
  for ( ; snapidx >= 0; snapidx--)
    markSnapshot(J, &J->cur.snap[snapidx], post_sink);

  for (ref = J->cur.nloop - 1; ref >= REF_FIRST; ref--)
    markIRIns(J, ref, post_sink);

  IF_DBG_LVL(3, printIRBuffer(J));

  /* 4. Replace all unmarked instructions by NOPs
  
     We also need to fix up the previous pointer and [J->chain.]
     Note, that we don't bother updating the [J->chain] entries for
     NOPs, though.  (We never traverse the NOP chain, so this avoids
     unnecessary work.)
  */

  /* 4.a

     In pre-allocation-sinking mode we are very conservative about
     which nodes to keep.  We don't know yet which allocations
     need to be sunken, so we have to be prepared for both cases:

       - if the allocation is sunken, we need PHI nodes for all
         fields, but not for the allocated pointer itself

       - if the allocation is NOT sunken we need a PHI node only
         for the allocated pointer.  (Some fields may require PHI
         nodes for other reasons, though)
     
     Thusly, wek keep a PHI node if any of its arguments is used.
     Also make sure that both arguments are marked.  i.e.,

       marked(PHI(a,b)) => marked(a) /\ marked(b)


     In post-allocation-sinking mode, we are more aggressive.  Since
     at this point we know exactly which allocations have been sunken.
     We "look through" sunken allocations and mark PHI nodes on the
     way (see [markSnapshot], [markIRIns]).  At the end, only marked
     PHI nodes are truly needed.

  */
  IRRef1 *phi_parent = &J->chain[IR_PHI];
  IRRef lastphi = J->cur.nins;
  IRRef next;

  LC_ASSERT(checkPerOpcodeLinks(J));

  for (ref = J->chain[IR_PHI]; ref; ref = next) {
    IRIns *ir = IR(ref);
    next = ir->prev;
    /* LOOP INVARIANTS: all PHIs + chain insertion */
    LC_ASSERT(ir->o == IR_PHI);
    LC_ASSERT(phi_parent != NULL && *phi_parent == ref);

    bool remove_phi, set_mark;
    
    if (post_sink) {
      remove_phi = !irt_getmark(ir->t);
      set_mark = false;
    } else {
      bool mark1 = irt_getmark(IR(ir->op1)->t);
      bool mark2 = irt_getmark(IR(ir->op2)->t);
      
      remove_phi = !(mark1 || mark2) || irt_type(ir->t) == IRT_VOID;
      set_mark = !(mark1 && mark2);
    }

    if (remove_phi) {     /* PHI node unnecessary */
      DBG_LVL(3, "DCE%c: Deleting PHI node: %d\n",
              post_sink ? '2' : '1',
              irref_int(ref));
      ir->o = IR_NOP;
      ir->t = IRT_VOID;

      *phi_parent = ir->prev;   /* remove from PHI chain */

    } else {
      DBG_LVL(3, "DCE: Keeping PHI node: %d\n", irref_int(ref));
      if (set_mark) { /* set mark on both if necessary */
        irt_setmark(IR(ir->op1)->t);
        irt_setmark(IR(ir->op2)->t);
      }
      phi_parent = &ir->prev;
      lastphi = ref;
    }
    irt_clearmark(ir->t);
  }

  /* Clear marks for all but the PHI chain (already handled above) */
  IRRef1 orig_phi_chain = J->chain[IR_PHI];
  memset(J->chain, 0, sizeof(J->chain));

  for (ref = REF_FIRST; ref < lastphi; ref++) {
    IRIns *ir = IR(ref);
    if (!irt_getmark(ir->t) && ir->o != IR_LOOP) {
      LC_ASSERT(ir->o != IR_PHI);
      DBG_PR("DCE: Deleting node %d\n", irref_int(ref));
      ir->o = IR_NOP;
      ir->t = IRT_VOID;
    } else {
      ir->prev = J->chain[ir->o];
      J->chain[ir->o] = ref;
    }
    irt_clearmark(ir->t);
  }

  /* Fix up chain for PHI */
  J->chain[IR_PHI] = orig_phi_chain;

  LC_ASSERT(checkPerOpcodeLinks(J));
}


#if 0
LC_FASTCALL void
deadPhiElim(JitState *J)
{
  IRRef ref, next;
  IRRef1 *phi_parent = &J->chain[IR_PHI];

  for (ref = J->chain[IR_PHI]; ref; ref = next) {
    IRIns *ir = IR(ref);
    next = ir->prev;
    /* LOOP INVARIANTS: all PHIs + chain insertion */
    LC_ASSERT(ir->o == IR_PHI);
    LC_ASSERT(phi_parent != NULL && *phi_parent == ref);
    
    bool sunken1 = IR(ir->op1)->o == IR_NEW && ir_issunken(IR(ir->op1));
    bool sunken2 = IR(ir->op2)->o == IR_NEW && ir_issunken(IR(ir->op2));
    
    DBG_LVL(3, "DPE: PHI %d => Sunken: %d=%d, %d=%d\n",
            irref_int(ref),
            irref_int(ir->op1), sunken1,
            irref_int(ir->op2), sunken2);
    /* LC_ASSERT((sunken1 == sunken2) && */
    /*           "Cannot have PHI node where only one node is sunken"); */
    if (sunken1 && sunken2) {
      DBG_LVL(3, "DPE: Deleting PHI node: %d\n", irref_int(ref));
      ir->o = IR_NOP;
      ir->t = IRT_VOID;

      *phi_parent = ir->prev;   /* remove from PHI chain */
    } else {
      /* DBG_LVL(3, "DPE: Keeping PHI node: %d\n", irref_int(ref)); */
      phi_parent = &ir->prev;
    }
  }
}
#endif

bool
checkPerOpcodeLinks(JitState *J)
{
  IRRef1 chain[IR__MAX];
  IRRef ref;
  bool ok = true;
  memset(chain, 0, sizeof(chain));
  for (ref = REF_FIRST; ref < J->cur.nins; ref++) {
    IRIns *ir = IR(ref);
    if (ir->prev != chain[ir->o] && ir->o != IR_NOP) {
      fprintf(stderr, "Inconsistent prev field for: %d (expected: %d, found: %d)\n",
              irref_int(ref), irref_int(chain[ir->o]), irref_int(ir->prev));
      printIR(&J->cur, *ir);
      ok = false;
    }
    chain[ir->o] = ref;
  }

  /* Dirty, dirty hack.  Ignore the NOP chain */
  chain[IR_NOP] = J->chain[IR_NOP];

  int o;
  for (o = IR_NOP + 1; o < IR__MAX; o++) {
    if (chain[o] != J->chain[o] &&
        o != IR_BASE && o != IR_KWORD && o != IR_HEAPCHK) {
      fprintf(stderr, "Chain entry for %s does not match. "
              "Got %d, expected %d\n",
              ir_name[o], irref_int(chain[o]),
              irref_int(J->chain[o]));
      ok = false;
    }
  }
  return ok;
}

/* Mark a sunken node by "looking through" it.

   The main reason is that we don't mark the PHI node for a sunken
   ref, but we *do* mark PHI nodes for its fields.
*/
LC_FASTCALL void
markSunkenNode(JitState *J, IRIns *ir, IRRef site)
{
  LC_ASSERT(ir->o == IR_NEW && ir_issunken(ir));
  /* If it the node is sunken, mark its fields instead.  These
     may be sunken as well. */
  HeapInfo *hpi = getHeapInfo(&J->cur, ir);
  int i;
  for (i = 0; i < hpi->nfields; i++) {
            
    IRRef ref = getHeapInfoField(&J->cur, hpi, i);
    IRIns *ir2 = IR(ref);
    DBG_LVL(3, "DCE2: Marking %d due to sunken %d, refsite = %d\n",
            irref_int(ref), irref_int(hpi->ref), irref_int(site));
    if (!irt_getmark(ir2->t)) {
      if (ir2->o == IR_NEW && ir_issunken(ir2)) {
        irt_setmark(ir2->t);
        markSunkenNode(J, ir2, site);
      }
      else
        markIRRef(J, ref, site);
    }
  }
}

INLINE_HEADER void
markSnapshot(JitState *J, SnapShot *snap, bool post_sink)
{
  int j;
  SnapEntry *se = &J->cur.snapmap[snap->mapofs];
  for (j = 0; j < snap->nent; j++, se++) {
    IRRef ref = snap_ref(*se);
    IRIns *ir = IR(ref);

    if (post_sink && ir->o == IR_NEW && ir_issunken(ir)) {
      irt_setmark(ir->t);
      markSunkenNode(J, ir, snap->ref);
    } else {
      markIRRef(J, ref, snap->ref);
    }
  }
}

INLINE_HEADER void
markIRIns(JitState *J, IRRef ref, bool post_sink)
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
      if (post_sink && ir_issunken(ir)) {
        irt_setmark(ir->t);
        markSunkenNode(J, ir, ref);
      } else {
        HeapInfo *h = &J->cur.heap[ir->op2];
        for (i = 0; i < h->nfields; i++) {
          markIRRef(J, getHeapInfoField(&J->cur, h, i), ref);
        }
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

/* Make PHI nodes contiguous (remove NOPs between PHIs).
 *
 * This is useful mainly for the IR interpreter.  Recall that PHI
 * nodes represent a *parallel* assignment.  Having all PHI nodes in
 * adjacent memory locations makes it easier to implement this in the
 * IR interpreter.
 */
LC_FASTCALL
void
compactPhis(JitState *J)
{
  IRRef ref, dstref, srcref;
  u2 nphis = 1;

  DBG_LVL(3, "Compacting PHIs\n");

  LC_ASSERT(checkPerOpcodeLinks(J));

  if (!J->chain[IR_PHI]) {      /* Trivial case. */
    J->cur.nphis = 0;
    return;
  }

  /* 1. Find first PHI. */
  for (ref = J->chain[IR_PHI]; IR(ref)->prev; ref = IR(ref)->prev)
    nphis++;

  srcref = ref;

  /* 1a. Try to skip past any NOPs while we're at it. */
  for (dstref = ref; IR(dstref - 1)->o == IR_NOP; dstref--)
    ;

  /* 2. Copy things */
  J->chain[IR_PHI] = 0;
  for ( ; srcref < J->cur.nins; srcref++) {
    DBG_LVL(3, "CPHI: First %d := %d\n",
            irref_int(dstref), irref_int(srcref));
    LC_ASSERT(dstref <= srcref);
    if (IR(srcref)->o != IR_PHI) {
      continue;
    } else if (dstref == srcref) {
      IR(dstref)->prev = J->chain[IR_PHI];
      J->chain[IR_PHI] = dstref;
      dstref++;
      continue;
    } else {
      *IR(dstref) = *IR(srcref);
      IR(dstref)->prev = J->chain[IR_PHI];
      J->chain[IR_PHI] = dstref;
      dstref++;
    }
  }
  /* 2. IR buffer may now be shorter.  There can't be anything after a
     PHI node at this point.  (The code generator may add something
     later, but that doesn't concern us here.)

     Resizing the buffer here avoids the need to set all opcodes to
     NOPs, too.
  */
  DBG_LVL(3, "CPHI: nphis = %d\n", (int)nphis);
  J->cur.nins = dstref;
  J->cur.nphis = nphis;

  LC_ASSERT(checkPerOpcodeLinks(J));
}

/*

Reorder PHI nodes into a sequencing-save order.

It is a common notational issue in literature about SSA form that PHI
nodes are separate and per-variable.  The problem is that semantically
all PHI nodes represent parallel assignment.  Our PHI nodes have the
same problem.  For example:

    PHI a b
    PHI c d
    PHI e a

This corresponds to the parallel assignment (or parallel move)

    (a, c, d) := (b, d, a)

But it is *not* equivalent to the following sequence of assignments:

    a := b
    c := d
    e := a   ; should be the value of "a" before the assignment above

If we moved "e := a" to the beginning of the sequence it would be a
valid sequence equivalent to the original parallel move.

In general there may be loops in which case we need one temporary
register (or two, one for integer values and one for floating point
values).

The algorithm used here is the imperative algorithm presented in the
paper:

    "Tilting at windmills with Coq: formal verification of a
    compilation algorithm for parallel moves" by Laurence Rideau,
    Bernard Paul Serpette, Xavier Leroy

We still represent moves as PHI nodes, but we need to be able to
represent moves to the temporary register.  We can just use another PHI
node for this.

 */

/*
Actually this is not needed here.  We need it in the code generator,
though.

void
reorderPhis(JitState *J)
{
  
}
*/


#endif
