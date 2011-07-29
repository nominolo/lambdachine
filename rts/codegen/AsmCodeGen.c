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
  MCode *mcp;		/* Current MCode pointer (grows down). */
  MCode *mclim;		/* Lower limit for MCode memory + red zone. */

  IRIns *ir;		/* Copy of pointer to IR instructions/constants. */
  JitState *J;		/* JIT compiler state. */

  x86ModRM mrm;		/* Fused x86 address operand. */

  SnapNo snapno;	/* Current snapshot number. */

  RegSet freeset;	/* Set of free registers. */

  i4 spill;	        /* Next spill slot. */

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

/* -- Main assembler routine ---------------------------------------------- */
void genAsm(JitState *J, Fragment *T) {
  ASMState as_;
  ASMState *as = &as_;

  /* Setup initial state. Copy some fields to reduce indirections. */
  as->J = J;
  as->T = T;
  as->ir = T->ir;
  as->curins = T->nins;

  as->mctop = reserveMCode(J, &as->mcbot);
  as->mcp   = as->mctop;
  as->mclim = as->mcbot + MCLIM_REDZONE;


  /* generate the exit stubs we need */
  asm_exitstub_setup(as, T->nsnap);


  /* generate some test code */
  as->snapno = 1;
  asm_guardcc(as, CC_E);
  emit_rr(as, XO_TEST, RID_ECX, RID_R12D);
  emit_movrr(as, IR(--as->curins), RID_EAX, RID_ECX);
  emit_movrr(as, IR(--as->curins), RID_EAX, RID_R12D);

  T->mcode = as->mcp;
  T->szmcode = (MSize)((char *)as->mctop - (char *)as->mcp);
  mcodeCommit(J, T->mcode);
}

#undef IR
