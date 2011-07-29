#ifndef _LAMBDACHINE_PRINT_CLOSURE_H
#define _LAMBDACHINE_PRINT_CLOSURE_H

#include "Bytecode.h"

#include <stdio.h>

void printClosure_(FILE *f, Closure* cl, int add_newline);
INLINE_HEADER void printClosure(Closure* cl) {
  printClosure_(stdout, cl, 1);
}
void printInfoTable(InfoTable* info0);

// Print a bytecode instruction.
//
// Returns: length of printed instrution (in multiples of BCIns).
u4 printInstruction(const BCIns *ins);
u4 printInstructionOneLine(const BCIns *ins);
void printCode(LcCode *code);
void printInlineBitmap(const BCIns *p0);

#endif
