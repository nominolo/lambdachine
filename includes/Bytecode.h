#ifndef _LAMBDACHINE_BYTECODE_H
#define _LAMBDACHINE_BYTECODE_H

#include "Common.h"

/*

Each instruction is 32 bits wide.  Some instructions may be followed
by a fields which are sized multiples of an instruction.

Instruction formats:

    MSB                   LSB
    +-----+-----+-----+-----+
    |  B  |  C  |  A  | OPC |  ABC format
    +-----+-----+-----+-----+
    |     D     |  A  | OPC |  AD format
    +-----------+-----+-----+

OPC, A, B and C are 8 bit fields.  D is 16 bits wide and overlaps B
and C.  We write SD when treating D as a signed field.

*/

#define BCMAX_A		0xff
#define BCMAX_B		0xff
#define BCMAX_C		0xff
#define BCMAX_D		0xffff

/* Calls with more arguments have to be translated into multiple calls
   (where the first ones are partial applications). */
#define BCMAX_CALL_ARGS  8

typedef u4 BCIns; /* A byte code instruction */
typedef u4 BCReg; /* Bytecode register. */
typedef u4 BCPos;

/* Macros to get instruction fields. */
#define bc_op(i)	(cast(BCOp, (i)&0xff))
#define bc_a(i)		(cast(BCReg, ((i)>>8)&0xff))
#define bc_b(i)		(cast(BCReg, (i)>>24))
#define bc_c(i)		(cast(BCReg, ((i)>>16)&0xff))
#define bc_d(i)		(cast(BCReg, (i)>>16))
#define bc_sd(i)	(cast(int, (i))>>16)
#define bc_j(i)		((ptrdiff_t)bc_d(i)-BCBIAS_J)
#define bc_case_tag(i)  (cast(u2, (i) & 0xffff))
#define bc_case_mintag(i) (cast(u2, (i) & 0xffff))
#define bc_case_maxtag(i) (cast(u2, ((i)>>16) & 0xffff))
#define bc_case_target(i)   ((ptrdiff_t)bc_case_maxtag(i))
#define bc_case_targetlo(i) ((ptrdiff_t)bc_case_mintag(i))

/* Extract the B/C field from a given D */
#define bc_b_from_d(d)  (cast(BCReg, (d) >> 8))
#define bc_c_from_d(d)  (cast(BCReg, (d) & 0xff))
#define bc_j_from_d(d)  ((ptrdiff_t)(d)-BCBIAS_J)

// Also needed by the assembler.
#define BCBIAS_J	0x8000

/*
Round bytes to multiples of instructions.  Rounds upwards, i.e.,

    BC_ROUND(bytes) * sizeof(BCIns) >= bytes

*/
#define BC_ROUND(bytes) \
  (((bytes) + (sizeof(BCIns) - 1)) / sizeof(BCIns))

/* Macros to encode instructions. */
#define BCINS_ABC(o, a, b, c) \
  (cast(BCIns, o)|(cast(BCIns, a)<<8)|\
  (cast(BCIns, b)<<24)|(cast(BCIns, c)<<16))
#define BCINS_AD(o, a, d) \
  (cast(BCIns, o)|(cast(BCIns, a)<<8)|(cast(BCIns, d)<<16))
#define BCINS_AJ(o, a, j)	BCINS_AD(o, a, (BCPos)((int32_t)(j)+BCBIAS_J))

/* Bytecode instruction definitions.  Order matters.

NOTE: When changing this, make sure to rebuild everything: compiler,
interpreter, and all bytecode files.

*/
#define BCDEF(_) \
  /* Comparison ops. Order significant. */ \
  _(ISLT,    RRJ) \
  _(ISGE,    RRJ) \
  _(ISLE,    RRJ) \
  _(ISGT,    RRJ) \
  _(ISEQ,    RRJ) \
  _(ISNE,    RRJ) \
  /* Unary ops */ \
  _(NOT,     RR) \
  _(NEG,     RR) \
  /* Updates */ \
  _(MOV,     RR) \
  _(MOV_RES, R) \
  _(UPDATE,  RR) \
  _(LOADF,   RRN) \
  _(LOADFV,  RN) \
  _(LOADBH,  R) \
  _(LOADSLF, R) \
  _(INITF,   RRN) /* Write to field (for initialisation) */   \
  /* Binary ops. */ \
  _(ADDRR,   RRR) \
  _(SUBRR,   RRR) \
  _(MULRR,   RRR) \
  _(DIVRR,   RRR) \
  _(REMRR,   RRR) \
  /* Constants */ \
  _(LOADK,   RN) \
  _(KINT,    RS) \
  _(NEW_INT, RS) \
  /* Allocation */ \
  _(ALLOC1,  ___) \
  _(ALLOC,   ___) \
  _(ALLOCAP, ___) \
  /* Calls and jumps */ \
  _(CALL,    ___) \
  _(CALLT,   ___) \
  _(RET1,    R) \
  _(JMP,     J) \
  _(EVAL,    ___) \
  _(CASE,    ___) \
  _(CASE_S,  ___) \
  /* Function headers */ \
  _(FUNC,    ___) \
  _(IFUNC,   ___) \
  _(JFUNC,   ___)



typedef enum {
#define BCENUM(name,fmt)  BC_##name,
BCDEF(BCENUM)
#undef BCENUM
  BC__MAX
} BCOp;

typedef enum {
  IFM_J,
  IFM_R,
  IFM_RR,
  IFM_RRR,
  IFM_RN,
  IFM_RRN,
  IFM_RS,
  IFM_RRJ,
  IFM____
} InsFormat;

extern InsFormat ins_format[];
extern const char *ins_name[];

// Can invert condition by toggling lowest bit.
LC_STATIC_ASSERT((BC_ISLT ^ 1) == BC_ISGE);
LC_STATIC_ASSERT((BC_ISGT ^ 1) == BC_ISLE);
LC_STATIC_ASSERT((BC_ISEQ ^ 1) == BC_ISNE);
// Order of comparison operations matters.  Same is enforced for IR.
LC_STATIC_ASSERT((BC_ISLT & 1) == 0);
LC_STATIC_ASSERT((BC_ISLT + 2) == BC_ISLE);
LC_STATIC_ASSERT((BC_ISLE + 2) == BC_ISEQ);

#endif
