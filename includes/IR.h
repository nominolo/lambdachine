#ifndef LC_IR_H
#define LC_IR_H

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



#endif /* LC_IR_H */
