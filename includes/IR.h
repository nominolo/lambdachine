#ifndef _LAMBDACHINE_IR_H
#define _LAMBDACHINE_IR_H

#include "Common.h"

typedef u2 IRRef1;              /* One stored reference */
typedef u4 IRRef2;              /* Two stored references */
typedef u4 IRRef;               /* Used to pass around references */

/*
 * LuaJIT IR
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
 */

typedef union IRIns {
  struct {
    LC_ENDIAN_LOHI(
      IRRef1 op1;               /* The first operand. */
    , IRRef1 op2;
    )
    u2 ot;
    IRRef1 prev;
  };
  struct {
    IRRef2 op12;
    LC_ENDIAN_LOHI(
      u1 t;
    , u1 o;
    )
    LC_ENDIAN_LOHI(
      u1 r;
    , u1 s;
    )
  };
  Word lit;
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
  _(LOOP,    N,   ___, ___) \
  _(PHI,     S,   ref, ref) \
  _(RENAME,  S,   ref, lit) \
  \
  _(KINT,    N,   cst, ___) \
  \
  _(EQ,      G,   ref, ref) \
  _(NE,      G,   ref, ref) \
  _(LT,      G,   ref, ref) \
  _(GE,      G,   ref, ref) \
  _(LE,      G,   ref, ref) \
  _(GT,      G,   ref, ref) \
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
  \
  _(FREF,    R,   ref, ref) \
  _(FLOAD,   L,   ref, ___) \
  _(SLOAD,   L,   lit, lit) \
  _(NEW,     A,   ref, lit)


#endif /* _LAMBDACHINE_IR_H */
