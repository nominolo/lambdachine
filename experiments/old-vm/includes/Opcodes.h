#ifndef _LAMBDACHINE_OPCODES_H
#define _LAMBDACHINE_OPCODES_H

/** Opcodes for byte code instructions *********************
 *
 * This is a separate file because these values are needed by the
 * assembly code (to initialise the dispatch table).  For the same
 * reason we also use #define instead of C enums.
 *
 */

#define OP_EXIT                 0
// Exits immediately.

// Operations on two unboxed integers (both in registers).
#define OP_ADDI_RR              1
#define OP_SUBI_RR              2
#define OP_MULI_RR              3
#define OP_DIVI_RR              4

#define OP_MOV                  20
#define OP_LOADK                21
// Loads a signed 16 bit constant.
#define OP_KSHORT               22

#define OP_JMP                  31

// Test instructions - must be followed by a jump.  If the test succeeds, the
// jump is executed, otherwise the jump instruction is skipped and
// execution continues after it.
//
// Furthermore, toggling the lowest bit must invert the condition.
#define OP_ISLT                 32
#define OP_ISGE                 33
#define OP_ISLE                 34
#define OP_ISGT                 35
#define OP_ISEQ                 36
#define OP_ISNE                 37

#define OP_RET1                 41
#define OP_CALL                 44
#define OP_TAILCALL             45
#define OP_IFUNC                46

// Also needed by the assembler.
#define BCBIAS_J	0x8000




#endif
