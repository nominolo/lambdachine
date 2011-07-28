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

#include <string.h> // for memset


/* -- Assembler state and common macros ----------------------------------- */

/* Assembler state. */
typedef struct ASMState {
  MCode *mcp;		/* Current MCode pointer (grows down). */
  MCode *mclim;		/* Lower limit for MCode memory + red zone. */

  IRIns *ir;		/* Copy of pointer to IR instructions/constants. */
  JitState *J;		/* JIT compiler state. */

  x86ModRM mrm;		/* Fused x86 address operand. */

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

/* -- Target-specific instruction emitter --------------------------------- */
#include "Emit_x64.h"

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

  emit_movrr(as, IR(--as->curins), RID_EAX, RID_ECX);
  emit_movrr(as, IR(--as->curins), RID_EAX, RID_R12D);

  T->mcode = as->mcp;
  T->szmcode = (MSize)((char *)as->mctop - (char *)as->mcp);
}

#undef IR
