#ifndef _LAMBDACHINE_PRINT_IR_H
#define _LAMBDACHINE_PRINT_IR_H

#include "IR.h"
#include "Jit.h"

void printIR(Fragment *, IRIns ir);
void printIRRef(Fragment *, IRRef1 ref);

void printPrettyIR_(FILE *out, Fragment *F, int fragment_id);
void printPrettyIRRef_(FILE *out, Fragment *F, IRRef ref, int follow);

INLINE_HEADER void
printPrettyIRRef(FILE *out, Fragment *F, IRRef ref)
{
  printPrettyIRRef_(out, F, ref, 1);
}

INLINE_HEADER void
printPrettyIR(Fragment *F, int fragment_id)
{
  printPrettyIR_(stderr, F, fragment_id);
}

#endif
