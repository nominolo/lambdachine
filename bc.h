#ifndef _LAMBDACHINE_BC_H
#define _LAMBDACHINE_BC_H

#include "def.h"

#include "opcodes.h"

/**

Instruction formats:

    MSB                   LSB
    +-----+-----+-----+-----+
    |  B  |  C  |  A  | OPC |  ABC format
    +-----+-----+-----+-----+
    |     D     |  A  | OPC |  AD format
    +-----------+-----+-----+

OPC, A, B and C are 8 bit fields.  D is 16 bits wide.

*/

/**
 * Opcodes for bytecode instructions 
 *
 */

#define BCMAX_A		0xff
#define BCMAX_B		0xff
#define BCMAX_C		0xff
#define BCMAX_D		0xffff

typedef uint32_t BCIns; /* A byte code instruction */
typedef uint32_t BCReg; /* Bytecode register. */
typedef uint32_t BCPos;
typedef int BCOp;       // TODO: should be an enum.


/* Macros to get instruction fields. */
#define bc_op(i)	(cast(BCOp, (i)&0xff))
#define bc_a(i)		(cast(BCReg, ((i)>>8)&0xff))
#define bc_b(i)		(cast(BCReg, (i)>>24))
#define bc_c(i)		(cast(BCReg, ((i)>>16)&0xff))
#define bc_d(i)		(cast(BCReg, (i)>>16))
#define bc_sd(i)	(cast(int, (i))>>16)
#define bc_j(i)		((ptrdiff_t)bc_d(i)-BCBIAS_J)

/* Macros to compose instructions. */
#define BCINS_ABC(o, a, b, c) \
  (cast(BCIns, o)|(cast(BCIns, a)<<8)|\
  (cast(BCIns, b)<<24)|(cast(BCIns, c)<<16))
#define BCINS_AD(o, a, d) \
  (cast(BCIns, o)|(cast(BCIns, a)<<8)|(cast(BCIns, d)<<16))
#define BCINS_AJ(o, a, j)	BCINS_AD(o, a, (BCPos)((int32_t)(j)+BCBIAS_J))

int BCIns_print(BCIns *ip, int);
void print_bytecode(BCIns *ip);
#endif
