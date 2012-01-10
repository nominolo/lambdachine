/*
** Definitions for x86 and x64 CPUs.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LAMBDACHINE_ASMTARGET_X64_H
#define _LAMBDACHINE_ASMTARGET_X64_H

/* -- Registers IDs ------------------------------------------------------- */
#define SLOT_SIZE sizeof(Word) /* size of stack slots */

/* -- Registers IDs ------------------------------------------------------- */

#define GPRDEF(_) \
  _(EAX) _(ECX) _(EDX) _(EBX) _(ESP) _(EBP) _(ESI) _(EDI) \
  _(R8D) _(R9D) _(R10D) _(R11D) _(R12D) _(R13D) _(R14D) _(R15D)
#define FPRDEF(_) \
  _(XMM0) _(XMM1) _(XMM2) _(XMM3) _(XMM4) _(XMM5) _(XMM6) _(XMM7) \
  _(XMM8) _(XMM9) _(XMM10) _(XMM11) _(XMM12) _(XMM13) _(XMM14) _(XMM15)

#define RIDENUM(name)	RID_##name,

enum {
  GPRDEF(RIDENUM)		/* General-purpose registers (GPRs). */
  FPRDEF(RIDENUM)		/* Floating-point registers (FPRs). */
  RID_MAX,
  RID_MRM = RID_MAX,		/* Pseudo-id for ModRM operand. */

  /* Fixed registers */
  RID_BASE = RID_EBP,
  RID_HP   = RID_R12D,

  /* Calling conventions. */
  RID_RET = RID_EAX,
  RID_FPRET = RID_XMM0,

  /* Register ranges [min, max) and number of registers. */
  RID_MIN_GPR = RID_EAX,
  RID_MIN_FPR = RID_XMM0,
  RID_MAX_GPR = RID_MIN_FPR,
  RID_MAX_FPR = RID_MAX,
  RID_NUM_GPR = RID_MAX_GPR - RID_MIN_GPR,
  RID_NUM_FPR = RID_MAX_FPR - RID_MIN_FPR,
};

/* -- Register sets ------------------------------------------------------- */

/* Make use of all registers, except the stack pointer. */
#define RSET_GPR	(RSET_RANGE(RID_MIN_GPR, RID_MAX_GPR)\
                                  -RID2RSET(RID_ESP)\
                                  -RID2RSET(RID_BASE)\
                                  -RID2RSET(RID_HP))
#define RSET_FPR	(RSET_RANGE(RID_MIN_FPR, RID_MAX_FPR))
#define RSET_ALL	(RSET_GPR|RSET_FPR)
#define RSET_INIT	RSET_ALL

/* Note: this requires the use of FORCE_REX! */
#define RSET_GPR8	RSET_GPR

/* ABI-specific register sets. */
#define RSET_ACD	(RID2RSET(RID_EAX)|RID2RSET(RID_ECX)|RID2RSET(RID_EDX))
/* The rest of the civilized x64 world has a common ABI. */
#define RSET_SCRATCH \
  (RSET_ACD|RSET_RANGE(RID_ESI, RID_R11D+1)|RSET_FPR)
#define REGARG_GPRS \
  (RID_EDI|((RID_ESI|((RID_EDX|((RID_ECX|((RID_R8D|(RID_R9D \
   <<5))<<5))<<5))<<5))<<5))
#define REGARG_NUMGPR	6
#define REGARG_FIRSTFPR	RID_XMM0
#define REGARG_LASTFPR	RID_XMM7
#define STACKARG_OFS	0

/* Prefer the low 8 regs of each type to reduce REX prefixes. 
 * This code will choose the highest bit in the regset that is set. The bswap
 * will put the available regs in the high bytes and fls will return the
 * highest index between 16-31 that is set. The xor with 0x18 translates this
 * back to the original bit index in the regset.
 */
#undef rset_picktop
#define rset_picktop(rs)	(lc_fls(lc_bswap(rs)) ^ 0x18)

/* Offset (in bytes) of HpLim from stack pointer inside trace. */
#define HPLIM_SP_OFFS   0

/* -- Spill slots --------------------------------------------------------- */

/* Spill slots are 64 bits wide.
*/
#define SPS_FIRST 1 /* first spill slot */
/* Scale a spill slot as an offset from the spill area.
 * The spill area is zero indexed (i.e. spills[0] is the first spill),
 * but  SPS_FIRST cannot  be zero because a zero means no spill slot is
 * allocated. As a result we simply don't use the first spill slot. This makes
 * it easy to index into spills[ir->s] without having to worry about
 * re-adjusting the index.*/
#define sps_scale(slot)  (sizeof(Word) * (int32_t)((slot)))
/* field refs start at index 1, which correctly skips the closure header*/
#define fref_scale(fref) (sizeof(Word) * (fref))
/* BASEO offset is from &base, but RID_BASE contains &base[-1] */
#define baseo_scale(ofs) ((sizeof(Word) * (ofs)) + sizeof(Word))

/* -- Exit state ---------------------------------------------------------- */

/* This definition must match with the asmEnter/asmExit functions */
typedef struct {
  double   fpr[RID_NUM_FPR];    /* Floating-point registers. */
  Word     gpr[RID_NUM_GPR];    /* General-purpose registers. */
  Word     *hplim;              /* Heap Limit */
  Word     *stacklim;           /* Stack Limit */
  Word     *spill;              /* Spill slots. */
  Thread   *T;                  /* Currently executing thread */
  Fragment *F;                  /* Fragment under execution */
} ExitState;

/* Limited by the range of a short fwd jump (127): (2+2)*(32-1)-2 = 122.
    The first exit stub in the group has to jump forward 122 bytes

    EXITSTUB_SPACING == size of an individual exit stub
      2 bytes for the push of high byte of exit number
      2 bytes for the relative jump to the end of the exit stub
      = 4 bytes per exit stub

    EXITSTUBS_PER_GROUP == number of exitstubs we can fit in one group
      Exit stubs are created in groups that can be reused by all traces
*/
#define EXITSTUB_SPACING	(2+2)
#define EXITSTUBS_PER_GROUP	32

/* -- x86 ModRM operand encoding ------------------------------------------ */

typedef enum {
  XM_OFS0 = 0x00, XM_OFS8 = 0x40, XM_OFS32 = 0x80, XM_REG = 0xc0,
  XM_SCALE1 = 0x00, XM_SCALE2 = 0x40, XM_SCALE4 = 0x80, XM_SCALE8 = 0xc0,
  XM_MASK = 0xc0
} x86Mode;

/* Structure to hold variable ModRM operand. */
typedef struct {
  int32_t ofs;		/* Offset. */
  uint8_t base;		/* Base register or RID_NONE. */
  uint8_t idx;		/* Index register or RID_NONE. */
  uint8_t scale;	/* Index scale (XM_SCALE1 .. XM_SCALE8). */
} x86ModRM;

/* -- Opcodes ------------------------------------------------------------- */

/* Macros to construct variable-length x86 opcodes. -(len+1) is in LSB. */
#define XO_(o)		((uint32_t)(0x0000fe + (0x##o<<24)))
#define XO_FPU(a,b)	((uint32_t)(0x00fd + (0x##a<<16)+(0x##b<<24)))
#define XO_0f(o)	((uint32_t)(0x0f00fd + (0x##o<<24)))
#define XO_66(o)	((uint32_t)(0x6600fd + (0x##o<<24)))
#define XO_660f(o)	((uint32_t)(0x0f66fc + (0x##o<<24)))
#define XO_f20f(o)	((uint32_t)(0x0ff2fc + (0x##o<<24)))
#define XO_f30f(o)	((uint32_t)(0x0ff3fc + (0x##o<<24)))

/* This list of x86 opcodes is not intended to be complete. Opcodes are only
** included when needed. Take a look at DynASM or jit.dis_x86 to see the
** whole mess.
*/
typedef enum {
  /* Fixed length opcodes. XI_* prefix. */
  XI_NOP =	0x90,
  XI_CALL =	0xe8,
  XI_JMP =	0xe9,
  XI_JMPs =	0xeb,
  XI_JCCs =	0x70, /* Really 7x. */
  XI_JCCn =	0x80, /* Really 0f8x. */
  XI_LEA =	0x8d,
  XI_MOVri =	0xb8, /* Really b8+r. */
  XI_ARITHib =	0x80,
  XI_ARITHi =	0x81,
  XI_ARITHi8 =	0x83,
  XI_PUSHi8 =	0x6a,
  XI_TEST =	0x85,
  XI_MOVmi =	0xc7,

  /* Note: little-endian byte-order! */
  XI_FLDZ =	0xeed9,
  XI_FLD1 =	0xe8d9,
  XI_FLDLG2 =	0xecd9,
  XI_FLDLN2 =	0xedd9,
  XI_FDUP =	0xc0d9,  /* Really fld st0. */
  XI_FPOP =	0xd8dd,  /* Really fstp st0. */
  XI_FPOP1 =	0xd9dd,  /* Really fstp st1. */
  XI_FRNDINT =	0xfcd9,
  XI_FSIN =	0xfed9,
  XI_FCOS =	0xffd9,
  XI_FPTAN =	0xf2d9,
  XI_FPATAN =	0xf3d9,
  XI_FSCALE =	0xfdd9,
  XI_FYL2X =	0xf1d9,

  /* Variable-length opcodes. XO_* prefix. */
  XO_MOV =	XO_(8b),
  XO_MOVto =	XO_(89),
  XO_MOVtow =	XO_66(89),
  XO_MOVtob =	XO_(88),
  XO_MOVmi =	XO_(c7),
  XO_MOVmib =	XO_(c6),
  XO_LEA =	XO_(8d),
  XO_ARITHib =	XO_(80),
  XO_ARITHi =	XO_(81),
  XO_ARITHi8 =	XO_(83),
  XO_ARITHiw8 =	XO_66(83),
  XO_SHIFTi =	XO_(c1),
  XO_SHIFT1 =	XO_(d1),
  XO_SHIFTcl =	XO_(d3),
  XO_IMUL =	XO_0f(af),
  XO_IMULi =	XO_(69),
  XO_IMULi8 =	XO_(6b),
  XO_CMP =	XO_(3b),
  XO_TEST =	XO_(85),
  XO_GROUP3b =	XO_(f6),
  XO_GROUP3 =	XO_(f7),
  XO_GROUP5b =	XO_(fe),
  XO_GROUP5 =	XO_(ff),
  XO_MOVZXb =	XO_0f(b6),
  XO_MOVZXw =	XO_0f(b7),
  XO_MOVSXb =	XO_0f(be),
  XO_MOVSXw =	XO_0f(bf),
  XO_MOVSXd =	XO_(63),
  XO_BSWAP =	XO_0f(c8),
  XO_CMOV =	XO_0f(40),

  XO_MOVSD =	XO_f20f(10),
  XO_MOVSDto =	XO_f20f(11),
  XO_MOVSS =	XO_f30f(10),
  XO_MOVSSto =	XO_f30f(11),
  XO_MOVLPD =	XO_660f(12),
  XO_MOVAPS =	XO_0f(28),
  XO_XORPS =	XO_0f(57),
  XO_ANDPS =	XO_0f(54),
  XO_ADDSD =	XO_f20f(58),
  XO_SUBSD =	XO_f20f(5c),
  XO_MULSD =	XO_f20f(59),
  XO_DIVSD =	XO_f20f(5e),
  XO_SQRTSD =	XO_f20f(51),
  XO_MINSD =	XO_f20f(5d),
  XO_MAXSD =	XO_f20f(5f),
  XO_ROUNDSD =	0x0b3a0ffc,  /* Really 66 0f 3a 0b. See asm_fpmath. */
  XO_UCOMISD =	XO_660f(2e),
  XO_CVTSI2SD =	XO_f20f(2a),
  XO_CVTSD2SI =	XO_f20f(2d),
  XO_CVTTSD2SI=	XO_f20f(2c),
  XO_CVTSI2SS =	XO_f30f(2a),
  XO_CVTSS2SI =	XO_f30f(2d),
  XO_CVTTSS2SI=	XO_f30f(2c),
  XO_CVTSS2SD =	XO_f30f(5a),
  XO_CVTSD2SS =	XO_f20f(5a),
  XO_ADDSS =	XO_f30f(58),
  XO_MOVD =	XO_660f(6e),
  XO_MOVDto =	XO_660f(7e),

  XO_FLDd =	XO_(d9), XOg_FLDd = 0,
  XO_FLDq =	XO_(dd), XOg_FLDq = 0,
  XO_FILDd =	XO_(db), XOg_FILDd = 0,
  XO_FILDq =	XO_(df), XOg_FILDq = 5,
  XO_FSTPd =	XO_(d9), XOg_FSTPd = 3,
  XO_FSTPq =	XO_(dd), XOg_FSTPq = 3,
  XO_FISTPq =	XO_(df), XOg_FISTPq = 7,
  XO_FISTTPq =	XO_(dd), XOg_FISTTPq = 1,
  XO_FADDq =	XO_(dc), XOg_FADDq = 0,
  XO_FLDCW =	XO_(d9), XOg_FLDCW = 5,
  XO_FNSTCW =	XO_(d9), XOg_FNSTCW = 7
} x86Op;

/* x86 opcode groups. */
typedef uint32_t x86Group;

#define XG_(i8, i, g)	((x86Group)(((i8) << 16) + ((i) << 8) + (g)))
#define XG_ARITHi(g)	XG_(XI_ARITHi8, XI_ARITHi, g)
#define XG_TOXOi(xg)	((x86Op)(0x000000fe + (((xg)<<16) & 0xff000000)))
#define XG_TOXOi8(xg)	((x86Op)(0x000000fe + (((xg)<<8) & 0xff000000)))

#define XO_ARITH(a)	((x86Op)(0x030000fe + ((a)<<27)))
#define XO_ARITHw(a)	((x86Op)(0x036600fd + ((a)<<27)))

typedef enum {
  XOg_ADD, XOg_OR, XOg_ADC, XOg_SBB, XOg_AND, XOg_SUB, XOg_XOR, XOg_CMP,
  XOg_X_IMUL
} x86Arith;

typedef enum {
  XOg_ROL, XOg_ROR, XOg_RCL, XOg_RCR, XOg_SHL, XOg_SHR, XOg_SAL, XOg_SAR
} x86Shift;

typedef enum {
  XOg_TEST, XOg_TEST_, XOg_NOT, XOg_NEG, XOg_MUL, XOg_IMUL, XOg_DIV, XOg_IDIV
} x86Group3;

typedef enum {
  XOg_INC, XOg_DEC, XOg_CALL, XOg_CALLfar, XOg_JMP, XOg_JMPfar, XOg_PUSH
} x86Group5;

/* x86 condition codes. */
typedef enum {
  CC_O, CC_NO, CC_B, CC_NB, CC_E, CC_NE, CC_BE, CC_NBE,
  CC_S, CC_NS, CC_P, CC_NP, CC_L, CC_NL, CC_LE, CC_NLE,
  CC_C = CC_B, CC_NAE = CC_C, CC_NC = CC_NB, CC_AE = CC_NB,
  CC_Z = CC_E, CC_NZ = CC_NE, CC_NA = CC_BE, CC_A = CC_NBE,
  CC_PE = CC_P, CC_PO = CC_NP, CC_NGE = CC_L, CC_GE = CC_NL,
  CC_NG = CC_LE, CC_G = CC_NLE
} x86CC;

#endif
