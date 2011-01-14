#ifndef _LAMBDACHINE_PRINT_CLOSURE_H
#define _LAMBDACHINE_PRINT_CLOSURE_H

#include "Bytecode.h"

void printClosure(Closure* cl);
void printInfoTable(InfoTable* info0);

// Print a bytecode instruction.
//
// Returns: length of printed instrution (in multiples of BCIns).
u4 printInstruction(BCIns *ins);
void printCode(LcCode *code);

#endif
