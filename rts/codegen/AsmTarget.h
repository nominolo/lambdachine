/*
** Definitions for target CPU.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LAMBDACHINE_ASMTARGET_H
#define _LAMBDACHINE_ASMTARGET_H

#include "Arch.h"
#include "Common.h"

/* -- Registers and spill slots ------------------------------------------- */

/* Register type (uint8_t in ir->r). */
typedef uint32_t Reg;

/* The hi-bit is NOT set for an allocated register. This means the value
** can be directly used without masking. The hi-bit is set for a register
** allocation hint or for RID_INIT.
*/
#define RID_NONE		0x80
#define RID_MASK		0x7f
#define RID_INIT		(RID_NONE|RID_MASK)

#define ra_noreg(r)		((r) & RID_NONE)
#define ra_hasreg(r)		(!((r) & RID_NONE))

/* Spill slot 0 means no spill slot has been allocated. */
#define SPS_NONE		0

#define ra_hasspill(s)		((s) != SPS_NONE)

/* Combined register and spill slot (uint16_t in ir->prev). */
typedef uint32_t RegSP;

#define REGSP(r, s)		((r) + ((s) << 8))
#define REGSP_HINT(r)		((r)|RID_NONE)
#define REGSP_INIT		REGSP(RID_INIT, 0)

#define regsp_reg(rs)		((rs) & 255)
#define regsp_spill(rs)		((rs) >> 8)
#define regsp_used(rs) \
  (((rs) & ~REGSP(RID_MASK, 0)) != REGSP(RID_NONE, 0))

/* -- Register sets ------------------------------------------------------- */

/* Bitset for registers. 32 registers suffice right now.
** Note that one set holds bits for both GPRs and FPRs.
*/
typedef uint32_t RegSet;

#define RID2RSET(r)		(((RegSet)1) << (r))
#define RSET_EMPTY		0
#define RSET_RANGE(lo, hi)	((RID2RSET((hi)-(lo))-1) << (lo))

#define rset_test(rs, r)	(((rs) >> (r)) & 1)
#define rset_set(rs, r)		(rs |= RID2RSET(r))
#define rset_clear(rs, r)	(rs &= ~RID2RSET(r))
#define rset_exclude(rs, r)	(rs & ~RID2RSET(r))
#define rset_picktop(rs)	((Reg)lc_fls(rs))
#define rset_pickbot(rs)	((Reg)lc_ffs(rs))

/* -- Target-specific definitions ----------------------------------------- */

#if LAMBDACHINE_TARGET == LAMBDACHINE_ARCH_X64
#include "AsmTarget_x64.h"
#else
#error "Missing include for target CPU"
#endif

/* Return the address of an exit stub. */
static LC_AINLINE MCode *exitstub_addr(JitState *J, ExitNo exitno)
{
  LC_ASSERT(J->exitstubgroup[exitno / EXITSTUBS_PER_GROUP] != NULL);
  return (MCode *)((char *)J->exitstubgroup[exitno / EXITSTUBS_PER_GROUP] +
		   EXITSTUB_SPACING*(exitno % EXITSTUBS_PER_GROUP));
}

#endif
