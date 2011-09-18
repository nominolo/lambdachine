/*
** IR assembler (SSA IR -> machine code).
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#include "Config.h"
#if LC_HAS_ASM_BACKEND

#include "AsmCodeGen.h"
#include "AsmTarget.h" // Target Machine Definitions
#include "Common.h"
#include "IR.h"
#include "Jit.h"
#include "MCode.h"
#include "InterpAsm.h" // to see asmExit function
#include "HeapInfo.h"
#include "MiscClosures.h" // for stg_IND_info

#include <string.h> // for memset


/* -- Assembler state and common macros ----------------------------------- */

/* Assembler state. */
typedef struct ASMState {
  RegCost cost[RID_MAX];/* Reference to IR instruction currently in the register 
                         *  and blended allocation cost for regs. */
  MCode *mcp;		/* Current MCode pointer (grows down). */
  MCode *mclim;		/* Lower limit for MCode memory + red zone. */

  IRIns *ir;		/* Copy of pointer to IR instructions/constants. */
  JitState *J;		/* JIT compiler state. */

  x86ModRM mrm;		/* Fused x86 address operand. */

  SnapNo snapno;	/* Current snapshot number. */
  IRRef snapref;	/* Current snapshot is active after this reference. */
  SnapNo loopsnapno;	/* Loop snapshot number. */

  RegSet freeset;	/* Set of free registers. */
  RegSet modset;	/* Set of modified registers. */

  RegSet phiset;	/* Set of registers assigned to RHS of PHI nodes. */
  IRRef1 phireg[RID_MAX];  /* Maps register to LHS PHI references. */

  i4 spill;             /* Next spill slot. */
  i4 spill_offset;      /* Offset from base reg into the spill area */

  IRRef curins;		/* Reference of current instruction. */
  IRRef stopins;	/* Stop assembly before hitting this instruction. */
  IRRef orignins;	/* Original T->nins. */

  Fragment *T;		/* Trace to assemble. */
  
  MCode *mcbot;		/* Bottom of reserved MCode. */
  MCode *mctop;		/* Top of generated MCode. */

} ASMState;

/* -- Helper Functions ---------------------------------------------------- */
#define IR(ref)			(&as->ir[(ref)])

/* Sparse limit checks using a red zone before the actual limit. */
#define MCLIM_REDZONE	64
#define checkmclim(as) \
  if (LC_UNLIKELY(as->mcp < as->mclim)) asm_mclimit(as)

static LC_NORET LC_NOINLINE void asm_mclimit(ASMState *as)
{
  //lj_mcode_limiterr(as->J, (size_t)(as->mctop - as->mcp + 4*MCLIM_REDZONE));
  LC_ASSERT(0 && "Asm code too big for trace");
  exit(1);
}

/* Check if a reference is a signed 32 bit constant. */
static int asm_isk32(ASMState *as, IRRef ref, int32_t *k)
{
  if (irref_islit(ref)) {
    IRIns *ir = IR(ref);
    if (ir->o == IR_KINT) {
      *k = ir->i;
      return 1;
    } else if (ir->o == IR_KWORD && checki32((Word)as->T->kwords[ir->u])) {
      *k = (int32_t)as->T->kwords[ir->u];
      return 1;
    }
  }
  return 0;
}

/* Map of comparisons to flags.
 * The comparison code is for the negated condition because that is the
 * condition on which we will exit the trace. For example, for IR_EQ we will
 * test for CC_NE so that we can generate a jne instruction that will jump off
 * the trace if the values are not equal
 */
static uint32_t asm_cmpmap(IROp opcode) {
  uint32_t cc;
  switch(opcode) {
    case IR_LT: cc = CC_GE; break;
    case IR_GE: cc = CC_L;  break;
    case IR_LE: cc = CC_G;  break;
    case IR_GT: cc = CC_LE; break;
    case IR_EQ: cc = CC_NE; break;
    case IR_NE: cc = CC_E;  break;
    default: LC_ASSERT(0 && "Unknown comparison opcode");
  }

  return  cc;
}

/* -- Instruction Emitter ------------------------------------------------- */
#include "Emit_x64.h"  // Target specific instruction emitter

/* -- Register Allocation ------------------------------------------------- */
/* Register allocation overview
 *  Allocation is performed during codegen in a backwards pass over the IR.
 *
 *  Each two operand instruction has the form
 *
 *  D = op L R
 *
 *  where D is the destination register L is the left operand and R is the
 *  right operand.
 *
 *  When we go to allocate registers for the instruction we do
 *  1. R - Make sure the right operand has a register.
 *     If R already has a register allocated then simply use that register
 *     If R does not have a register allocated we need to choose one
 *       If there is a free register available then use it
 *       If there are no more registers available then evict a register
 *         Evicting a register will assign a spill slot to the IR that is spilled
 *
 *         Generate a load for the spilled instruction. We generate a load now
 *         because we are allocating bottom up and we already generated
 *         instructions that expected to find the spilled value in a particular
 *         register. We will generate the store for the spilled value when we
 *         get to its defition point which is earlier in the instruction
 *         stream.
 *  2. D - Make sure the destination has a register allocated
 *       The D may already have a register allocated if it was referenced a an L
 *       or R value in the instructions below.
 *
 *       If it has a register allocated then reuse it, Otherwise find a
 *       register to use as for R
 *
 *       If D was assigned a spill slot earlier then generate a store to spill
 *       the regsiter to its assigned slot. We do this so that the later load
 *       will find the register in the right location.
 *
 *       Mark the register used by D as free. Since we going bottom up, the D
 *       register will be free at all the instructions above the current op,
 *       since D is defined in this instruction.
 *
 *  3. op - Generate the code for the operation using D = op D R
 *
 *  4. L - Now generate code to make sure that the L IRRef is in the same
 *  register as D. We may need to generate a register-to-register copy here.
 *  This is a required step for all of the x86 two operand machine
 *  instructions.
 *
 *  Each one operand instruction has the form of
 *
 *  D = op R
 *
 *  Code is generated similarly for the two-operand case except that L is not
 *  needed.
 */
#include "RA_debug.h"
#define ra_free(as, r)		rset_set(as->freeset, (r))
#define ra_modified(as, r)	rset_set(as->modset, (r))
#define ra_used(ir)		(ra_hasreg((ir)->r) || ra_hasspill((ir)->s))


/* Setup register allocator. */
static void ra_setup(ASMState *as)
{
  Reg r;
  /* Initially all regs (except the base and heap pointers) are free for use. */
  as->freeset = RSET_INIT;
  as->modset = RSET_EMPTY;
  as->phiset = RSET_EMPTY;
  memset(as->phireg, 0, sizeof(as->phireg));
  for (r = RID_MIN_GPR; r < RID_MAX; r++)
    as->cost[r] = REGCOST(~0u, 0u);
}

/* Rematerialize constants. */
static Reg ra_rematk(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  Reg r = ir->r;
  LC_ASSERT(ra_hasreg(r) && !ra_hasspill(ir->s));
  ra_free(as, r);
  ra_modified(as, r);
  ir->r = RID_INIT;  /* Do not keep any hint. */
  RA_DBGX((as, "remat     $i $r", ir, r));

  if (ir->o == IR_KINT) {
    emit_loadi(as, r, ir->i);
  }
  else if (ir->o == IR_KWORD) {
    emit_loadu64(as, r|REX_64, as->T->kwords[ir->u]);
  } else if(ir->o == IR_KBASEO) {
    emit_rmro(as, XO_LEA, r|REX_64, RID_BASE, baseo_scale(ir->i));
  } else {
    LC_ASSERT(0 && "Cannot remat ir instruction");
  }
  return r;
}

/* Force a spill. Allocate a new spill slot if needed. */
static int32_t ra_spill(ASMState *as, IRIns *ir)
{
  int32_t slot = ir->s;
  if (!ra_hasspill(slot)) {
    slot = as->spill;
    as->spill++;

    if (as->spill > 256) {
      LC_ASSERT(0 && "Too Many Spills");
      traceError(as->J, 1);
    }
    ir->s = (uint8_t)slot;
  }
  return (as->spill_offset) + sps_scale(slot);
}

/* Restore a register (marked as free). Rematerialize or force a spill. */
static Reg ra_restore(ASMState *as, IRRef ref)
{
  if (emit_canremat(ref)) {
    return ra_rematk(as, ref);
  } else {
    IRIns *ir = IR(ref);
    int32_t ofs = ra_spill(as, ir);  /* Force a spill slot. */
    Reg r = ir->r;
    LC_ASSERT(ra_hasreg(r));
    ra_sethint(ir->r, r);  /* Keep hint. */
    ra_free(as, r);
    ra_modified(as, r);
    RA_DBGX((as, "restore   $i $r", ir, r));
    emit_spload(as, ir, r, ofs);
    return r;
  }
}
/* Save a register to a spill slot. */
static void ra_save(ASMState *as, IRIns *ir, Reg r)
{
  RA_DBGX((as, "save      $i $r", ir, r));
  emit_spstore(as, ir, r, (as->spill_offset) + sps_scale(ir->s));
}

/* Evict the register with the lowest cost, forcing a restore. */
#define MINCOST(name) \
  if (rset_test(RSET_ALL, RID_##name) && \
      LC_LIKELY(allow&RID2RSET(RID_##name)) && as->cost[RID_##name] < cost) \
    cost = as->cost[RID_##name];
static Reg ra_evict(ASMState *as, RegSet allow)
{
  IRRef ref;
  RegCost cost = ~(RegCost)0;
  LC_ASSERT(allow != RSET_EMPTY);
  if (allow < RID2RSET(RID_MAX_GPR)) {
    GPRDEF(MINCOST)
  } else {
    FPRDEF(MINCOST)
  }
  ref = regcost_ref(cost);
  LC_ASSERT((ref >= as->T->nk && ref < as->T->nins));
  return ra_restore(as, ref);
}

/* Pick any register (marked as free). Evict on-demand. */
static Reg ra_pick(ASMState *as, RegSet allow)
{
  RegSet pick = as->freeset & allow;
  if (!pick)
    return ra_evict(as, allow);
  else
    return rset_picktop(pick);
}

/* Get a scratch register (marked as free). */
static Reg ra_scratch(ASMState *as, RegSet allow)
{
  Reg r = ra_pick(as, allow);
  ra_modified(as, r);
  return r;
}

/* Pick a destination register (marked as free).
** Caveat: allow is ignored if there's already a destination register.
*/
static Reg ra_dest(ASMState *as, IRIns *ir, RegSet allow)
{
  Reg dest = ir->r;
  if (ra_hasreg(dest)) {
    ra_free(as, dest);
    ra_modified(as, dest);
  } else {
    if (ra_hashint(dest) && rset_test(as->freeset, ra_gethint(dest))) {
      dest = ra_gethint(dest);
      ra_modified(as, dest);
    } else {
      dest = ra_scratch(as, allow);
    }
    ir->r = dest;
  }
  RA_DBGX((as, "dest           $r", dest));
  if (LC_UNLIKELY(ra_hasspill(ir->s))) ra_save(as, ir, dest);
  return dest;
}

/* Allocate a register for ref from the allowed set of registers.
** Note: this function assumes the ref does NOT have a register yet!
** Picks an optimal register, sets the cost and marks the register as non-free.
*/
static Reg ra_allocref(ASMState *as, IRRef ref, RegSet allow)
{
  IRIns *ir = IR(ref);
  RegSet pick = as->freeset & allow;
  Reg r;
  LC_ASSERT(ra_noreg(ir->r));
  if (pick) {
    /* First check register hint from propagation or PHI. */
    if (ra_hashint(ir->r)) {
      r = ra_gethint(ir->r);
      if (rset_test(pick, r))  /* Use hint register if possible. */
	goto found;
      /* Rematerialization is cheaper than missing a hint. */
      if (rset_test(allow, r) && emit_canremat(regcost_ref(as->cost[r]))) {
	ra_rematk(as, regcost_ref(as->cost[r]));
	goto found;
      }
      RA_DBGX((as, "hintmiss  $f $r", ref, r));
    }
    /* If we get here then we did not have a hint, or it wasn't available */
    r = rset_pickbot(pick);
  } else { /* No regs available */
    r = ra_evict(as, allow);
  }
found:
  RA_DBGX((as, "alloc     $f $r", ref, r));
  ir->r = (uint8_t)r;
  rset_clear(as->freeset, r);
  as->cost[r] = REGCOST_REF_T(ref, ir->t);
  return r;
}

static Reg ra_allock(ASMState *as, IRRef ref, RegSet allow, int32_t *k) {
  LC_ASSERT(irref_islit(ref) && "Non literal given to allock");
  Reg r = RID_NONE;

  /* If the constant can't fit in 32 bits then allocate a register */
  if(!asm_isk32(as, ref, k)) {
    r = IR(ref)->r = ra_scratch(as, allow);
  }

  return r;
}

/* Allocate a register on-demand. */
static Reg ra_alloc(ASMState *as, IRRef ref, RegSet allow)
{
  Reg r = IR(ref)->r;
  /* Note: allow is ignored if the register is already allocated. */
  if (ra_noreg(r)){r = ra_allocref(as, ref, allow);}
  else{RA_DBGX((as, "existing  $f $r", ref, r));}
  return r;
}

/* Propagate dest register to left reference. Emit moves as needed.
** This is a required fixup step for all 2-operand machine instructions.
*/
static void ra_left(ASMState *as, Reg dest, IRRef lref)
{
  IRIns *ir = IR(lref);
  Reg left  = ir->r;
  if(ra_noreg(left)) {
    if(irref_islit(lref)) {
      int32_t k;
      if(asm_isk32(as, lref, &k)) {
        emit_loadi(as, dest, k);
      }
      else {
        ir->r = dest;
        ra_rematk(as, lref);
      }
      return;
    }
    if (!ra_hashint(left))
        ra_sethint(ir->r, dest);  /* Propagate register hint. */
    left = ra_allocref(as, lref, dest < RID_MAX_GPR ? RSET_GPR : RSET_FPR);
  }
  /* Move needed for true 3-operand instruction: y=a+b ==> y=a; y+=b. */
  if (dest != left) {
    emit_movrr(as, dest, left);
  }
}
/* -- Exit stubs ---------------------------------------------------------- */
/* Generate an exit stub group at the bottom of the reserved MCode memory.

    Machine code state before. The address values are just to orient the
    picture. Remeber that mctop grows down during code generation.

    0x00000 ----------- <- mcbot
            | REDZONE |
    0x00040 ----------- <- mclim
            |         |
            |         |
            |         |
            |         |
            |         |
    0x10000 ----------- <- mctop

    After generating th exit stubs the machine code area looks like the picture
    below further calls to exitstub_gen will and a new exit stub group at the
    location pointed to by mcbot.

            ----------- <- asm_exitstub_gen return value points here
            | EXITGRP0|
            ----------- <- mcbot
            | REDZONE |
            ----------- <- mclim
            |         |
            |         |
            |         |
            |         |
            |         |
            ----------- <- mctop

    Each exit stub group contains a number of exit stubs controlled by the
    EXITSTUBS_PER_GROUP constant. An exit stub is responsible for indicating
    which gaurd failed so that the correct snapshot can be restored when
    returning to the interpreter. A exit stub is setup as follows. Assume that
    we are generating exit stubs for group number 1

    push $0  #first push is the exit number in the group
    jmp  END
    push $1
    jmp  END
    ...
    push $31
    jmp  END

    END:
    push $1 # second push is the exit group number
    jmp asmExit # The exit routine that will restore the interpreter
                # state according to the exit number pushed on the stack

    To keep the exit stubs a consistent size, we use two 2-byte opcodes to
    implement each exit stub. The first opcode pushes a byte sized value onto
    the c-stack. The second is a relative jump to the end of the exit group.

*/
static MCode *asm_exitstub_gen(ASMState *as, ExitNo group)
{
  ExitNo i, groupofs = (group*EXITSTUBS_PER_GROUP) & 0xff;
  MCode *mxp = as->mcbot;
  MCode *mxpstart = mxp;
  if (mxp + (2+2)*EXITSTUBS_PER_GROUP+8+5 >= as->mctop)
    asm_mclimit(as);
  /* Push low byte of exitno for each exit stub. */
  *mxp++ = XI_PUSHi8; *mxp++ = (MCode)groupofs;
  for (i = 1; i < EXITSTUBS_PER_GROUP; i++) {
    *mxp++ = XI_JMPs; *mxp++ = (MCode)((2+2)*(EXITSTUBS_PER_GROUP - i) - 2);
    *mxp++ = XI_PUSHi8; *mxp++ = (MCode)(groupofs + i);
  }
  /* Push the high byte of the exitno for each exit stub group. */
  *mxp++ = XI_PUSHi8; *mxp++ = (MCode)((group*EXITSTUBS_PER_GROUP)>>8);
  /* Jump to exit handler which fills in the ExitState. */
  *mxp++ = XI_JMP; mxp += 4;
  *((int32_t *)(mxp-4)) = jmprel(mxp, (MCode *)(void *)asmExit);
  /* Commit the code for this group (even if assembly fails later on). */
  mcodeCommitBot(as->J, mxp);
  as->mcbot = mxp;
  as->mclim = as->mcbot + MCLIM_REDZONE;
  return mxpstart;
}

static void asm_exitstub_setup(ASMState *as, ExitNo nexits) {
  ExitNo i;
  if (nexits >= EXITSTUBS_PER_GROUP*LC_MAX_EXITSTUBGR) {
    LC_ASSERT(0 && "Too many exit stubs!");
    traceError(as->J, 1);
  }
  for (i = 0; i < (nexits+EXITSTUBS_PER_GROUP-1)/EXITSTUBS_PER_GROUP; i++)
    if (as->J->exitstubgroup[i] == NULL)
      as->J->exitstubgroup[i] = asm_exitstub_gen(as, i);
}

/* Allocate register or spill slot for a ref that escapes to a snapshot. */
static void asm_snap_alloc1(ASMState *as, IRRef ref)
{
  IRIns *ir = IR(ref);
  if (!ra_used(ir)) {
    RegSet allow = RSET_GPR;
    /* Get a weak register if we have a free one or can rematerialize. */
    if ((as->freeset & allow)){
      ra_allocref(as, ref, allow);  /* Allocate a register. */
      checkmclim(as);
      RA_DBGX((as, "snapreg   $f $r", ref, ir->r));
    } else {
      ra_spill(as, ir);  /* Otherwise force a spill slot. */
      RA_DBGX((as, "snapspill $f $s", ref, ir->s));
    }
  }
}

/* Allocate refs escaping to a snapshot. */
static void asm_snap_alloc(ASMState *as)
{
  RA_DBGX((as, "<<SNAP $x>>", as->snapno));
  SnapShot *snap = &as->T->snap[as->snapno];
  SnapEntry *map = &as->T->snapmap[snap->mapofs];
  u1 n, nent = snap->nent;
  for (n = 0; n < nent; n++) {
    SnapEntry sn = map[n];
    IRRef ref = snap_ref(sn);
    if (!irref_islit(ref)) {
      asm_snap_alloc1(as, ref);
    }
  }
}

/* Prepare snapshot for next guard instruction. */
static void asm_snap_prep(ASMState *as)
{
  if (as->curins < as->snapref) {
    do {
      LC_ASSERT(as->snapno != 0);
      as->snapno--;
      as->snapref = as->T->snap[as->snapno].ref;
    } while (as->curins < as->snapref);
    asm_snap_alloc(as);
  }
}

/* Emit conditional branch to exit for guard.
** It's important to emit this *after* all registers have been allocated,
** because rematerializations may invalidate the flags.
*/
static void asm_guardcc(ASMState *as, int cc)
{
  MCode *target = exitstub_addr(as->J, as->snapno);
  emit_jcc(as, cc, target);
}

/* -- Trace setup --------------------------------------------------------- */

/* Clear reg/sp for all instructions and add register hints. */
static void asm_setup_regsp(ASMState *as)
{
  Fragment *T = as->T;
  IRRef i, nins;

  ra_setup(as);
  as->spill_offset = SLOT_SIZE * T->framesize;

  /* Clear reg/sp for constants. */
  for (i = T->nk; i < REF_BIAS; i++)
    IR(i)->prev = REGSP_INIT;

  /* REF_BASE is used for implicit references to the BASE register. */
  IR(REF_BASE)->prev = REGSP_HINT(RID_BASE);

  nins = T->nins;
  as->snapref = nins;
  as->snapno = T->nsnap;

  as->stopins = REF_BASE;
  as->orignins = nins;
  as->curins = nins;
  as->spill = SPS_FIRST;

  for (i = REF_FIRST; i < nins; i++) {
    IRIns *ir = IR(i);
    switch (ir->o) {
      /* Non-constant shift counts need to be in RID_ECX on x86/x64. */
      case IR_BSHL: case IR_BSHR: case IR_BSAR: case IR_BROL: case IR_BROR:
        if (!irref_islit(ir->op2) && !ra_hashint(IR(ir->op2)->r)) {
          IR(ir->op2)->r = REGSP_HINT(RID_ECX);
        }
        break;
      default:
        ir->prev = REGSP_INIT;
        break;
    }
  }
}

/* -- PHI and loop handling  ---------------------------------------------- */

/* Emit rename ir instruction. */
static void emit_ir_rename(ASMState *as, Reg down, IRRef ref, SnapNo snapno) {
    IRRef ren;
    ren = tref_ref(emit_raw(as->J, IRT(IR_RENAME, IRT_VOID), ref, snapno));
    as->ir = as->T->ir;  /* The IR may have been reallocated. */
    IR(ren)->r = (uint8_t)down;
    IR(ren)->s = SPS_NONE;
}

/* Rename register allocation and emit move.
 *
 * This function takes care of the operations needed when we change the
 * register allocated to an ir instruction. For example, consider this code
 *
 *     ... = x
 *     ...
 *     --- as->curins --
 *     ... = r1 ;; allocation = {x : r1}
 *     ...
 *     GUARD(snapno=3) ;; snapshot = {0 : x}
 *
 * If we call rename(as, r1, r2), we will end up the following state
 *
 *     ... = x
 *     ...
 *     	;; allocation = {x : r2}
 *     --- as->curins --
 *     r1  = r2
 *     ... = r1
 *     GUARD(snapno=3) {0 : x}
 *     IR_RENAME(x, 3)
 *
 * The allocation state has been updated, a rr move inserted, and the rename ir
 * instruction inserted to make sure that we restore x from the correct
 * location when the guard fails.
 **/
static void ra_rename(ASMState *as, Reg down, Reg up)
{
  IRRef ref = regcost_ref(as->cost[up] = as->cost[down]);
  IRIns *ir = IR(ref);
  ir->r = (uint8_t)up;
  as->cost[down] = 0;
  LC_ASSERT((down < RID_MAX_GPR) == (up < RID_MAX_GPR));
  LC_ASSERT(!rset_test(as->freeset, down) && rset_test(as->freeset, up));
  ra_free(as, down);  /* 'down' is free ... */
  ra_modified(as, down);
  rset_clear(as->freeset, up);  /* ... and 'up' is now allocated. */
  RA_DBGX((as, "rename    $f $r $r", regcost_ref(as->cost[up]), down, up));
  emit_movrr(as, down, up);  /* Backwards codegen needs inverse move. */
  if (!ra_hasspill(IR(ref)->s)) {  /* Add the rename to the IR. */
    emit_ir_rename(as, down, ref, as->snapno);
  }
}

/* Break a PHI cycle by renaming to a free register (evict if needed).
 * This gets rid of cycles caused by code like this:
 *
 *     ... = x0
 *     y1  = ...
 *     ... = y0
 *     x1  = ...
 *     phi(x0, x1)
 *     phi(y0, y1)
 *
 * With the allocation like:
 *     { x1 : r1,  y1 : r2
 *     	 y0 : r1,  x0 : r2 }
 *
 * To break the cycle we choose one of the blockers and rename it to a free
 * register.
 */
static void asm_phi_break(ASMState *as, RegSet blocked, RegSet blockedby,
			  RegSet allow)
{
  RegSet candidates = blocked & allow;
  if (candidates) {  /* If this register file has candidates. */
    /* Note: the set for ra_pick cannot be empty, since each register file
    ** has some registers never allocated to PHIs.
    */
    Reg down, up = ra_pick(as, ~blocked & allow);  /* Get a free register. */
    if (candidates & ~blockedby)  /* Optimize shifts, else it's a cycle. */
      candidates = candidates & ~blockedby;
    down = rset_picktop(candidates);  /* Pick candidate PHI register. */
    ra_rename(as, down, up);  /* And rename it to the free register. */
  }
}

/* PHI register shuffling.
 *
 * The allocator tries hard to preserve PHI register assignments across
 * the loop body. Most of the time this loop does nothing, since there
 * are no register mismatches.
 *
 * A mismatch occurs when the LHS of a phi node has a different register than
 * the RHS. If there is a mismatch then we need to insert some copies to make
 * sure that the correct value flows through the loop. Inserting a copy is
 * called renaming the register becuase the IR instruction will be allocated to
 * two different registers over the course of the trace. When we rename the
 * register we take that register to use outside the loop and free the current
 * register for use outside the loop.
 *
 * As a small example, consider this code
 *      x1  = x0 + 1
 *      ... = x0
 *      phi(x0, x1)
 *
 * With this register assignment: { x1 : r1,  x0 : r2 }
 *
 * If r1 is free outside the loop, then we can "rename" the register for x0, by
 * changing the assignment to use r1 outside the loop and r2 inside the loop.
 * This renaming also ensures that the phi operation (x0 = phi(x0,x1)) happens
 * correctly.
 *
 * To rename we can insert a copy from `r1` to `r2` at the top of the loop and
 * assign `r1` to `x0` outside of the loop. Since `r1` is on free on entry to the
 * loop we can safely take it for use by `x0` outside of the loop. The final code
 * will look like
 *
 *      r1  = ... (initialze x0)
 *      LOOP:
 *      	r2  = r1
 *      ;; body
 *      	r1  = r2 + 1
 *      	... = r2
 *      goto LOOP
 *
 * The action taken for a register mismatch depends on the state of the
 * register allocated to the value in the RHS of the phi node.
 *
 * If a register mismatch is detected and ...
 * - the register is currently free: rename it.
 * - the register is blocked by an invariant: restore/remat and rename it.
 * - Otherwise the register is used by another PHI, so mark it as blocked.
 *
 * The renames are order-sensitive, so just retry the loop if a register
 * is marked as blocked, but has been freed in the meantime. A cycle is
 * detected if all of the blocked registers are allocated. To break the
 * cycle rename one of them to a free register and retry.
 *
 * Note that PHI spill slots are kept in sync and don't need to be shuffled.
*/
static void asm_phi_shuffle(ASMState *as){
  RA_DBGX((as, "<<PHI SHUFFLE>>"));
  RegSet work;

  /* Find and resolve PHI register mismatches. */
  for (;;) {
    RegSet blocked = RSET_EMPTY;
    RegSet blockedby = RSET_EMPTY;
    RegSet phiset = as->phiset;
    while (phiset) {  /* Check all left PHI operand registers. */
      Reg r = rset_pickbot(phiset);
      IRIns *irl = IR(as->phireg[r]);
      Reg left = irl->r;
      if (r != left) {  /* Mismatch? */
	if (!rset_test(as->freeset, r)) {  /* PHI register blocked? */
	  IRRef ref = regcost_ref(as->cost[r]);
	  /* Blocked by other PHI (w/reg)? */
	  if (!irref_islit(ref) && irt_getmark(IR(ref)->t)) {
	    rset_set(blocked, r);
	    if (ra_hasreg(left))
	      rset_set(blockedby, left);
	    left = RID_NONE;
	  } else {  /* Otherwise grab register from invariant. */
	    ra_restore(as, ref);
	    checkmclim(as);
	  }
	}
	if (ra_hasreg(left)) {
	  ra_rename(as, left, r);
	  checkmclim(as);
	}
      }
      rset_clear(phiset, r);
    }
    if (!blocked) break;  /* Finished. */
    if (!(as->freeset & blocked)) {  /* Break cycles if none are free. */
      asm_phi_break(as, blocked, blockedby, RSET_GPR);
      checkmclim(as);
    }  /* Else retry some more renames. */
  }/* end for */

  /* Restore/remat invariants whose registers are modified inside the loop. */
  work = as->modset & ~(as->freeset | as->phiset);
  while (work) {
    Reg r = rset_pickbot(work);
    ra_restore(as, regcost_ref(as->cost[r]));
    rset_clear(work, r);
    checkmclim(as);
  }

  /* For any LHS that was spilled inside the loop, make sure that
   * we store the current value at the top of the loop so that the spill slot
   * will hold the current value when it is loaded in the loop body.
   **/
  work = as->phiset;
  while (work) {
    Reg r = rset_picktop(work);
    IRRef lref = as->phireg[r];
    IRIns *ir = IR(lref);
    if (ra_hasspill(ir->s)) {  /* Left PHI gained a spill slot? */
      irt_clearmark(ir->t);  /* Handled here, so clear marker now. */
      /* Get a register for the store operation. Either the LHS already has a
       * register from the loop (it can be live out even thought it has a
       * spill) or we need to assign a new register. If we need a new register
       * then use the same register as the RHS of the phi. This register will
       * be free for us to use because the loop above will ensure it is so
       * beacuse the RID_NONE != rhs assignment.*/
      ra_alloc(as, lref, RID2RSET(r));
      ra_save(as, ir, r);  /* Save to spill slot inside the loop. */
      checkmclim(as);
    }
    rset_clear(work, r);
  }
}

/* Setup right PHI reference. */
static void asm_phi(ASMState *as, IRIns *ir) {
  RA_DBGX((as, "<<PHI $f $f>>", ir->op1, ir->op2));
  RegSet allow = RSET_GPR & ~as->phiset;
  RegSet afree = as->freeset & allow;
  IRIns *irl = IR(ir->op1);
  IRIns *irr = IR(ir->op2);

  /* Spill slot shuffling is not implemented yet (but rarely needed). */
  if (ra_hasspill(irl->s) || ra_hasspill(irr->s)) {
    LC_ASSERT(0 && "Spill slot shuffling not implemented");
  }

  /* Leave at least one register free for non-PHIs (and PHI cycle breaking).*/
  if((afree & (afree-1))) { /* Two or more free registers? */
    Reg r;
    if (ra_noreg(irr->r)) { /* Get a register for the right PHI. */
      r = ra_allocref(as, ir->op2, allow);
    }
    else { /* Duplicate right PHI, need a copy (rare). */
      r = ra_scratch(as, allow);
      emit_movrr(as, r, irr->r);
    }
    ir->r = (uint8_t)r;
    rset_set(as->phiset, r);
    as->phireg[r] = (IRRef1)ir->op1;
    irt_setmark(irl->t); /* Marks left PHIs that have a register in rhs. */
    if (ra_noreg(irl->r)) {
      ra_sethint(irl->r, r); /* Set register hint for left PHI. */
    }
  }
  else {  /* Otherwise allocate a spill slot. */
    /* This is overly restrictive, but it triggers only on synthetic code. */
    if (ra_hasreg(irl->r) || ra_hasreg(irr->r)){
      LC_ASSERT(0 && "Unable to spill phi node with already assigned regs");
    }
    ra_spill(as, ir);
    irl->s = irr->s = ir->s;  /* Sync left/right PHI spill slots. */
  }
}

/* Emit renames for LHS phis which are only spilled outside the loop.
 * We need to do this because if the LHS was spilled outside of the loop the
 * snapshot restoration will attempt to load it from the spill slot. This spill
 * slot will be out of date as the value changes in the loop. We emit a
 * rename to ensure that snapshots will restore the value from the register
 * inside the loop.*/
static void asm_phi_spill_fixup(ASMState *as)
{
  RegSet work = as->phiset;
  while (work) {
    Reg r = rset_picktop(work);
    IRRef lref = as->phireg[r];
    IRIns *ir = IR(lref);
    /* Left PHI gained a spill slot before the loop? */
    if (irt_getmark(ir->t) && ra_hasspill(ir->s)) {
      emit_ir_rename(as, r, lref, as->loopsnapno);
    }
    irt_clearmark(ir->t);  /* Always clear marker. */
    rset_clear(work, r);
  }
}

/* Prepare tail of trace code */
static void asm_tail_prep(ASMState *as) {
  MCode *p = as->mctop;
  p -= 5;  /* Space for exit branch (near jmp). */
  as->mcp = p;
}

/* Fixup the loop branch at end of trace to jump to current target. */
static void asm_loop_fixup(ASMState *as) {
  MCode *p = as->mctop;
  MCode *target = as->mcp;

  p[-5] = XI_JMP;
  *(int32_t *)(p-4) = (int32_t)(target - p);
}


/* Middle part of a loop. */
static void asm_loop(ASMState *as)
{
  /* Record the last snapshot we passed. We need this to store in the IR_RENAME
   * instructions*/
  as->loopsnapno = as->snapno;

  /* LOOP marks the transition from the variant to the invariant part.
   * Values that are defined outside the loop and do not flow into a PHI node
   * will not change anytime during exection. The values defined in the loop
   * and those that reach phi nodes may change as the loop executes.
   *
   * When we reach the loop marker we need to resolve any differences in
   * allocation of values in the same phi node and then fixup the backwards
   * branch to point to the start of the loop.*/

  /* reslove differences in phi node allocation */
  asm_phi_shuffle(as);

  /* fixup the backward branch at bottom of trace to jump to top of loop */
  asm_loop_fixup(as);
  RA_DBGX((as, "===== LOOP =====")); RA_DBG_FLUSH();
}

/* -- Allocations --------------------------------------------------------- */
static void
asm_heapstore(ASMState *as, IRRef ref, int32_t ofs, Reg base, RegSet allow){
  if(irref_islit(ref)) {
    int32_t k;
    Reg r = ra_allock(as, ref, allow, &k);
    if(ra_noreg(r)) {
      emit_movmroi(as, base, ofs, k);
    }
    else {
      emit_rmro(as, XO_MOVto, REX_64|r, REX_64|base, ofs);
      ra_rematk(as, ref);
    }
  }
  else {
    Reg r = ra_alloc(as, ref, allow);
    emit_rmro(as, XO_MOVto, REX_64|r, REX_64|base, ofs);
  }
}

static void asm_hpalloc(ASMState *as, uint32_t bytes) {
  emit_gri(as, XG_ARITHi(XOg_ADD), RID_HP|REX_64, bytes);
}

/* -- Specific instructions  ---------------------------------------------- */
static void asm_update(ASMState *as, IRIns *ir) {
  RA_DBGX((as, "<<UPDATE $f $f>>", ir->op1, ir->op2));
  /* set first field of the indirection as a pointer to other closure */
  Reg old = ra_alloc(as, ir->op1, RSET_GPR);
  asm_heapstore(as, ir->op2, sizeof(Word) * wordsof(ClosureHeader), old, RSET_GPR);

  /* update the info table to an indirection */
  Reg r = ra_scratch(as, RSET_GPR);
  emit_rmro(as, XO_MOVto, REX_64|r, REX_64|old, 0);
  emit_loadu64(as, r|REX_64, (Word)&stg_IND_info);
}

static void asm_new(ASMState *as, IRIns *ir) {
    RA_DBGX((as, "<<NEW $f>>", ir->op1));
    Fragment *F  = as->T;
    HeapInfo *hp = &F->heap[ir->op2];
    int j;
    int32_t ofs = wordsof(ClosureHeader) * sizeof(Word);
    int32_t numBytes = sizeof(Word) * (wordsof(ClosureHeader) + hp->nfields);

    /* allocate a register for the result */
    RegSet allow = RSET_GPR;
    Reg dest = ra_dest(as, ir, allow);
    rset_clear(allow, dest);

    /* do the allocation by bumping heap pointer */
    asm_hpalloc(as, numBytes);

    /* store each field as an offset from the pre-bumped heap pointer */
    for (j = 0; j < hp->nfields; j++) {
      IRRef ref = getHeapInfoField(F, hp, j);
      asm_heapstore(as, ref, ofs, dest, allow);
      ofs += sizeof(Word);
    }

    /* store the closure header */
    asm_heapstore(as, ir->op1, 0, dest, allow);

    /* save current heap pointer as the result of the allocation */
    emit_movrr(as, dest, RID_HP);
}

static void asm_intarith(ASMState *as, IRIns *ir, x86Arith xa) {
  RA_DBGX((as, "<<ARITH $f $f>>", ir->op1, ir->op2));
  RegSet allow = RSET_GPR;
  int32_t k = 0;
  IRRef lref = ir->op1;
  IRRef rref = ir->op2;

  Reg right = IR(rref)->r;
  if(ra_hasreg(right)) {
    rset_clear(allow, right);
  }
  Reg dest  = ra_dest(as, ir, allow);

  if(irref_islit(rref)) {
    right = ra_allock(as, rref, rset_clear(allow, dest), &k);
  }
  else { /* Non-constant right operand */
    right = ra_alloc(as, rref, rset_clear(allow, dest));
  }

  if(xa != XOg_X_IMUL) {
    if(ra_hasreg(right)) {
      emit_mrm(as, XO_ARITH(xa), dest|REX_64, right|REX_64);

      /* load the 64-bit constant into the reg before use */
      if(irref_islit(rref)) {
        ra_rematk(as, rref);
      }
    }
    else { /* 32-bit constant */
      emit_gri(as, XG_ARITHi(xa), dest|REX_64, k);
    }
  }
  else { /* Integer Multiplication */
    if(ra_hasreg(right)) { /* IMUL r, mrm */
      emit_mrm(as, XO_IMUL, dest|REX_64, right|REX_64);

      /* load the 64-bit constant into the reg before use */
      if(irref_islit(rref)) {
        ra_rematk(as, rref);
      }
    }
    else { /* IMUL r, r, k */
      Reg left = ra_alloc(as, lref, RSET_GPR);
      x86Op xo;
      if (checki8(k)) { emit_i8(as, k); xo = XO_IMULi8;
      } else { emit_i32(as, k); xo = XO_IMULi; }
      emit_mrm(as, xo, dest|REX_64, left|REX_64);
      return;
    }
  }

  ra_left(as, dest, lref);
}

static void asm_fload(ASMState *as, IRIns *ir) {
  RA_DBGX((as, "<<FLOAD $f>>", ir->op1));

  int need_remat = 0;
  RegSet allow = RSET_GPR;
  Reg base;
  Reg dest = ra_dest(as, ir, allow);
  rset_clear(allow, dest);

  /* Allocate a register for the base pointer of the fload */
  IRIns *irf = IR(ir->op1);
  LC_ASSERT((irf->o == IR_FREF) && "FLOAD must use FREF as first arguement");
  if(irref_islit(irf->op1)) {
    int k;
    base = ra_allock(as, irf->op1, allow, &k);
    LC_ASSERT(ra_hasreg(base) && "Must have base register for fref");
    need_remat = 1;
  }
  else {
    base = ra_alloc(as, irf->op1, allow);
  }

  /* setup the [base + ofs * idx] operand which is the fref*/
  int32_t ofs  = irf->op2;
  as->mrm.base = base;
  as->mrm.ofs  = fref_scale(ofs);
  as->mrm.idx  = RID_NONE;

  /* emit load */
  emit_mrm(as, XO_MOV, dest|REX_64, RID_MRM);

  /* emit remat  if needed */
  if(need_remat) ra_rematk(as, irf->op1);
}

static void asm_fref(ASMState *as, IRIns *ir) {
  RA_DBGX((as, "<<FREF $f 0x$x>>", ir->op1, ir->op2));
  int32_t ofs = ir->op2;
  Reg dest = ra_dest(as, ir, RSET_GPR);
  if(irref_islit(ir->op1)) {
    int k;
    Reg base = ra_allock(as, ir->op1, rset_exclude(RSET_GPR, dest), &k);
    if(ra_hasreg(base)) {
      emit_rmro(as, XO_LEA, dest|REX_64, base|REX_64, fref_scale(ofs));
      ra_rematk(as, ir->op1); /* load the constant into the reg before use */
    }
    else {
      LC_ASSERT(0 && "Cannot use constant in FREF");
    }

  }
  else { // non-lit
    Reg base = ra_alloc(as, ir->op1, rset_exclude(RSET_GPR, dest));
    emit_rmro(as, XO_LEA, dest|REX_64, base|REX_64, fref_scale(ofs));
  }
}

static void asm_cmp(ASMState *as, IRIns *ir, uint32_t cc) {
  RA_DBGX((as, "<<CMP   $f $f>>", ir->op1, ir->op2));
  Reg left  = ra_alloc(as, ir->op1, RSET_GPR);
  IRRef rref = ir->op2;
  if(irref_islit(rref)) {
    int32_t imm;
    Reg right = ra_allock(as, rref, rset_exclude(RSET_GPR, left), &imm);
    if(ra_noreg(right)) { /* 32-bit constant */
      asm_guardcc(as, cc);
      emit_gmrmi(as, XG_ARITHi(XOg_CMP), left|REX_64, imm);
    }
    else { /* have to use a register to hold the 64-bit constant */
      asm_guardcc(as, cc);
      emit_mrm(as, XO_CMP, left|REX_64, right|REX_64);
      ra_rematk(as, rref); /* load the constant into the reg before use */
    }
  }
  else { /* Non-constant right op */
    Reg right = ra_alloc(as, rref, rset_exclude(RSET_GPR, left));
    asm_guardcc(as, cc);
    emit_mrm(as, XO_CMP, left|REX_64, right|REX_64);
  }
}

static void asm_iload(ASMState *as, IRIns *ir) {
  RA_DBGX((as, "<<ILOAD $f>>", ir->op1));
  int32_t ofs = 0; /* info table is at offset 0 of closure */
  RegSet allow = RSET_GPR;
  Reg dest    = ra_dest(as,  ir, allow);
  Reg base    = ra_alloc(as, ir->op1, allow);

  emit_rmro(as, XO_MOV, dest|REX_64, base, ofs);
}

static void asm_sload(ASMState *as, IRIns *ir)
{
  RA_DBGX((as, "<<SLOAD 0x$x>>", ir->op1));
  int32_t ofs = SLOT_SIZE * ir->op1;
  Reg base = RID_BASE;
  RegSet allow = RSET_GPR;
  Reg dest = ra_dest(as, ir, allow);
  emit_rmro(as, XO_MOV, dest|REX_64, base, ofs);
}

/* -- Main assembler routine ---------------------------------------------- */
/* Assemble a single instruction */
static void asm_ir(ASMState *as, IRIns *ir) {
  switch((IROp)ir->o) {
    case IR_SLOAD:
      asm_sload(as, ir);
      break;
    case IR_ILOAD:
      asm_iload(as, ir);
      break;
    case IR_EQ: case IR_LT: case IR_GE: case IR_LE: case IR_GT: case IR_NE:
      asm_cmp(as, ir, asm_cmpmap(ir->o));
      break;
    case IR_FREF:
      asm_fref(as, ir);
      break;
    case IR_FLOAD:
      asm_fload(as, ir);
      break;
    case IR_ADD:
      asm_intarith(as, ir, XOg_ADD);
      break;
    case IR_SUB:
      asm_intarith(as, ir, XOg_SUB);
      break;
    case IR_MUL:
      asm_intarith(as, ir, XOg_X_IMUL);
      break;
    case IR_NEW:
      asm_new(as, ir);
      break;
    case IR_UPDATE:
      asm_update(as, ir);
      break;
    case IR_PHI:
      asm_phi(as, ir);
      break;
    case IR_LOOP:
      asm_loop(as);
      break;
    case IR_FRAME: case IR_NOP: case IR_RET:
      break;
    default:
      LC_ASSERT(0 && "IR op not implemented");
  }

}

/* Assemble a trace Fragment */
void genAsm(JitState *J, Fragment *T) {
  ASMState as_;
  ASMState *as = &as_;

  /* Setup initial state. Copy some fields to reduce indirections. */
  as->J = J;
  as->T = T;
  as->ir = T->ir;

  /* Allocate and setup machine code area */
  as->mctop = reserveMCode(J, &as->mcbot);
  as->mcp   = as->mctop;
  as->mclim = as->mcbot + MCLIM_REDZONE;

  /* generate the exit stubs we need */
  asm_exitstub_setup(as, T->nsnap);

  /* generate the tail of the trace that will loop back or exit */
  asm_tail_prep(as);

  /* Setup initial register and spill state */
  asm_setup_regsp(as);

  /* generate code in linear backwards order need */
  RA_DBG_START();
  as->stopins = REF_BASE;
  as->curins  = T->nins;
  for(as->curins--; as->curins > as->stopins; as->curins--) {
    IRIns *ir = IR(as->curins);
    // Disable dce for now to debug the codegen
    //if (!ra_used(ir) && !ir_sideeff(ir)){
    //  continue;  /* Dead-code elimination can be soooo easy. */
    //}
    if (irt_isguard(ir->t)){ asm_snap_prep(as); }
    asm_ir(as, ir);
  }

  /* Add renames for any phis spilled outside the loop */
  asm_phi_spill_fixup(as);

  /* Sanity check. Should never happen. */
  if (as->freeset != RSET_ALL)
    LC_ASSERT(0 && "Oops. All registers have not been freed");

  RA_DBGX((as, "===== START ===="));
  RA_DBG_FLUSH();

  /* Save the trace's machine code */
  T->mcode = as->mcp;
  T->szmcode = (MSize)((char *)as->mctop - (char *)as->mcp);
  mcodeCommit(J, T->mcode);
  RA_DBG_FLUSH();
}

#undef IR

#endif
