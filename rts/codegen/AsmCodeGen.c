/*
** IR assembler (SSA IR -> machine code).
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#include "AsmCodeGen.h"
#include "AsmTarget.h" // Target Machine Definitions
#include "Common.h"
#include "IR.h"
#include "Jit.h"
#include "MCode.h"
#include "InterpAsm.h" // to see asmExit function

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

  RegSet freeset;	/* Set of free registers. */
  RegSet modset;	/* Set of modified registers. */

  i4 spill;             /* Next spill slot. */
  i4 spill_offset;      /* Offset from base reg into the spill area */

  IRRef curins;		/* Reference of current instruction. */
  IRRef stopins;	/* Stop assembly before hitting this instruction. */
  IRRef orignins;	/* Original T->nins. */

  Fragment *T;		/* Trace to assemble. */
  
  MCode *mcbot;		/* Bottom of reserved MCode. */
  MCode *mctop;		/* Top of generated MCode. */

} ASMState;

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

/* Setup register allocator. */
static void ra_setup(ASMState *as)
{
  /* Initially all regs (except the base and heap pointers) are free for use. */
  as->freeset = RSET_INIT;
  as->modset = RSET_EMPTY;
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
    emit_rmro(as, XO_LEA, r|REX_64, RID_BASE, sps_scale(ir->i));
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
  emit_spstore(as, ir, r, sps_scale(ir->s));
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
    return rset_pickbot(pick);
}

/* Get a scratch register (marked as free). */
static Reg ra_scratch(ASMState *as, RegSet allow)
{
  Reg r = ra_pick(as, allow);
  ra_modified(as, r);
  RA_DBGX((as, "scratch        $r", r));
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
      RA_DBGX((as, "dest           $r", dest));
    } else {
      dest = ra_scratch(as, allow);
    }
    ir->r = dest;
  }
  if (LC_UNLIKELY(ra_hasspill(ir->s))) ra_save(as, ir, dest);
  return dest;
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

/* -- Specific instructions  ---------------------------------------------- */
static void asm_sload(ASMState *as, IRIns *ir)
{
  RA_DBGX((as, "SLOAD 0x$x", ir->op1));
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

  /* Setup initial register and spill state */
  asm_setup_regsp(as);

  /* generate the exit stubs we need */
  asm_exitstub_setup(as, T->nsnap);

  /* generate code in linear backwards order need */
  RA_DBG_START();
  as->stopins = REF_BASE;
  as->curins  = T->nins;
  as->curins  = REF_FIRST + 1; // for testing
  for(as->curins--; as->curins > as->stopins; as->curins--) {
    IRIns *ir = IR(as->curins);
    asm_ir(as, ir);
  }

  /* generate some test code */
  /*
  as->snapno = 1;
  asm_guardcc(as, CC_E);
  emit_rr(as, XO_TEST, RID_ECX, RID_R12D);
  emit_movrr(as, IR(--as->curins), RID_EAX, RID_ECX);
  emit_movrr(as, IR(--as->curins), RID_EAX, RID_R12D);
  */

  T->mcode = as->mcp;
  T->szmcode = (MSize)((char *)as->mctop - (char *)as->mcp);
  mcodeCommit(J, T->mcode);
  RA_DBG_FLUSH();
}

#undef IR
