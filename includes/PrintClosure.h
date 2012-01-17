#ifndef _LAMBDACHINE_PRINT_CLOSURE_H
#define _LAMBDACHINE_PRINT_CLOSURE_H

#include "Bytecode.h"

#include <stdio.h>

void printClosure_(FILE *f, Closure* cl, bool add_newline);
INLINE_HEADER void printClosure(Closure* cl) {
  printClosure_(stderr, cl, true);
}
void printInfoTable(FILE *stream, InfoTable* info0);

// Print a bytecode instruction.
//
// Returns: length of printed instruction (in multiples of BCIns).
u4 printInstruction_aux(FILE *stream, const BCIns *ins /*in*/, int oneline);

INLINE_HEADER u4
printInstruction(FILE *stream, const BCIns *ins) {
  return printInstruction_aux(stream, ins, 0);
}

INLINE_HEADER u4
printInstructionOneLine(FILE *stream, const BCIns *ins) {
  return printInstruction_aux(stream, ins, 1);
}

void printCode(FILE *stream, LcCode *code);
void printInlineBitmap(FILE *stream, const BCIns *p0);

#endif
