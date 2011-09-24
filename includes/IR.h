#ifndef _LAMBDACHINE_IR_H
#define _LAMBDACHINE_IR_H

#include "Common.h"

typedef u2 IRRef1;              /* One stored reference */
typedef u4 IRRef2;              /* Two stored references */
typedef u4 IRRef;               /* Used to pass around references */

#define mkIRRef2(r1, r2) ((r1) | ((r2) << 16))
#define irref2_ref1(r)   ((u2)(r))
#define irref2_ref2(r)   ((u2)((r) >> 16))

/*
 * LuaJIT IR
 *
 * LSB                           MSB
 *
 *    16      16     8   8   8   8
 * +-------+-------+---+---+---+---+
 * |  op1  |  op2  | t | o | r | s |
 * +-------+-------+---+---+---+---+
 * |  op12/i/gco   |  ot   | prev  |
 * +---------------+-------+-------+
 *        32          16      16
 *
 *   o .. opcode
 *   t .. IR type + flags
 *   r .. register allocation
 *   s .. spill slot allocation
 *
 *   prev .. Points to previous instruction with same opcode (or 0).
 *           Used for fast CSE.  Only valid before register allocation.
 */

typedef union IRIns {
  struct {
    LC_ENDIAN_LOHI(
      IRRef1 op1;               // First operand.
    , IRRef1 op2;               // Second operand
    )
    u2 ot;                      // Opcode + type
    IRRef1 prev;                // Previous instruction of same opcode
  };
  struct {
    IRRef2 op12;                // Unified operands.  (op1 is low)
    LC_ENDIAN_LOHI(
      u1 t;
    , u1 o;
    )
    LC_ENDIAN_LOHI(
      u1 r;
    , u1 s;
    )
  };

  // TODO: I think we need more LC_ENDIAN_LOHI to make sure that these
  // overlap with op1/2, not ot/prev

  i4 i;   // A literal 32bit integer
  u4 u;   // Index into J->kwords
} IRIns;

// A side effect of this design is that we have to use two IR instructions
// per store, because a store takes three arguments (base pointer,
// offset, and value).  A store is therefore encoded as:
//
//     ref  FREF   base offs   ; reference to a field
//     void FSTORE ref value
//
// Flags:
//   N .. normal
//   C .. commutative
//   G .. guard
//   A .. alloc
//
// Operand Types:
//   ref .. IR reference
//   lit .. 16 bit unsigned literal
//   cst .. 32/64bit constant (pointer, int)
//   none .. unised operand
//
#define IRDEF(_) \
  _(NOP,     N,   ___, ___) \
  _(BASE,    N,   lit, lit) \
  _(FRAME,   S,   lit, lit) \
  _(RET,     S,   lit, lit) \
  _(LOOP,    N,   ___, ___) \
  _(PHI,     N,   ref, ref) \
  _(RENAME,  S,   ref, lit) \
  \
  _(KINT,    N,   cst, ___) \
  _(KWORD,   N,   cst, ___) \
  _(KBASEO,  N,   cst, ___) \
  \
  _(LT,      G,   ref, ref) \
  _(GE,      G,   ref, ref) \
  _(LE,      G,   ref, ref) \
  _(GT,      G,   ref, ref) \
  _(EQ,      G,   ref, ref) \
  _(NE,      G,   ref, ref) \
   \
  _(BNOT,    N,   ref, ___) \
  _(BAND,    C,   ref, ref) \
  _(BOR,     C,   ref, ref) \
  _(BXOR,    C,   ref, ref) \
  _(BSHL,    N,   ref, ref) \
  _(BSHR,    N,   ref, ref) \
  _(BSAR,    N,   ref, ref) \
  _(BROL,    N,   ref, ref) \
  _(BROR,    N,   ref, ref) \
  \
  _(ADD,     C,   ref, ref) \
  _(SUB,     N,   ref, ref) \
  _(MUL,     C,   ref, ref) \
  _(DIV,     N,   ref, ref) \
  _(REM,     N,   ref, ref) \
  \
  _(FREF,    R,   ref, lit) \
  _(FLOAD,   L,   ref, ___) \
  _(SLOAD,   L,   lit, lit) \
  _(ILOAD,   L,   ref, ___) \
  _(RLOAD,   L,   ___, ___) \
  _(NEW,     A,   ref, lit) \
  _(FSTORE,  S,   ref, ref) \
  _(UPDATE,  S,   ref, ref)

typedef enum {
#define IRENUM(name,flags,arg1,arg2) IR_##name,
  IRDEF(IRENUM)
#undef IRENUM
  IR__MAX
} IROp;

// Can invert condition by toggling lowest bit.
LC_STATIC_ASSERT((IR_LT ^ 1) == IR_GE);
LC_STATIC_ASSERT((IR_GT ^ 1) == IR_LE);
LC_STATIC_ASSERT((IR_EQ ^ 1) == IR_NE);
// Order of comparison operations matters.  Same is enforced for bytecode.
LC_STATIC_ASSERT((IR_LT & 1) == 0);
LC_STATIC_ASSERT((IR_LT + 2) == IR_LE);
LC_STATIC_ASSERT((IR_LE + 2) == IR_EQ);

typedef enum {
  IRMref,  // IR reference
  IRMlit,  // 16 bit unsigned literal
  IRMcst,  // Constant literal (int, ptr, ...)
  IRMnone // Unused operand
} IRMode;
#define IRM___   IRMnone

// Flags
//
// TODO: We probably want something different here than LuaJIT
#define IRM_C     0x10
#define IRM_N     0x00
#define IRM_R     IRM_N
#define IRM_A     0x20
#define IRM_L     0x40
#define IRM_S     0x60
#define IRM_G     0x80

// TODO: add flags to ir_mode info
#define IRMODE(name, flags, m1, m2) \
  (((IRM##m1)|(IRM##m2)<<2)|IRM_##flags),

extern const u1 ir_mode[IR__MAX + 1];
extern const char *ir_name[];

#define irm_op1(m)     (cast(IRMode, (m) & 3))
#define irm_op2(m)     (cast(IRMode, ((m) >> 2) & 3))
#define irm_iscomm(m)  ((m) & IRM_C)

typedef enum {
  IRT_UNK,
  IRT_VOID,  // No result
  IRT_I32,
  IRT_U32,
  IRT_CHAR,
  IRT_F32,

  IRT_CLOS,  // Pointer to a closure
  IRT_INFO,  // Pointer to an info table
  IRT_PC,    // Program counter
  IRT_PTR,   // Other pointer

  // Flags
  IRT_MARK  = 0x20,  // Marker for various purposes
  IRT_ISPHI = 0x40,  // Used by register allocator
  IRT_GUARD = 0x80,  // Used by asm code generator

  // Masks
  IRT_TYPE = 0x1f,
  IRT_T    = 0xff
} IRType;

#define IRT_CMP (IRT_VOID|IRT_GUARD)

#define irt_setmark(irt) ((irt) |= IRT_MARK)
#define irt_getmark(irt) ((irt) & IRT_MARK)
#define irt_clearmark(irt)  ((irt) &= ~IRT_MARK)
#define irt_setphi(irt)  ((irt) |= IRT_ISPHI)
#define irt_getphi(irt)  ((irt) & IRT_ISPHI)
#define irt_clearphi(irt)   ((irt) &= ~IRT_ISPHI)
#define irt_type(irt)    ((irt) & IRT_TYPE)
#define irt_isguard(irt) ((irt) & IRT_GUARD)

enum {
  REF_BIAS  = 0x8000,
  REF_BASE  = REF_BIAS,
  REF_FIRST = REF_BIAS + 1,
  REF_DROP  = 0xffff
};

#define irref_islit(ref)   ((ref) < REF_BIAS)
#define irref_int(ref)     ((int)((ref) - REF_BIAS))

#define IRT(o, t)      (cast(u4, ((o) << 8) | (t)))

// Tagged IR reference.
//
// MSB                           LSB
// +-------+-------+---------------+
// |  type | flags |      ref      |
// +-------+-------+---------------+
//
// TODO: Not sure we need the type info.
//
typedef u4 TRef;

#define TREF_REFMASK   0x0000ffff

#define TREF(ref, t)   ((TRef)((ref) + ((t)<<24)))

#define tref_ref(tr)   ((IRRef1)(tr))
#define tref_t(tr)     ((IRType)((tr) >> 24))

/* A store or any other op with a guard has a side-effect. */
static LC_AINLINE int ir_sideeff(IRIns *ir)
{
  return ((irt_isguard(ir->t)) ||  (ir_mode[ir->o] & IRM_S));
}

static LC_AINLINE int ir_issunken(IRIns *ir)
{
  return !irt_getmark(ir->t);
}

#endif /* _LAMBDACHINE_IR_H */
