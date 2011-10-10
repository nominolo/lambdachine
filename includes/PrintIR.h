#ifndef _LAMBDACHINE_PRINT_IR_H
#define _LAMBDACHINE_PRINT_IR_H

#include "IR.h"
#include "Jit.h"

void printIR(Fragment *, IRIns ir);
void printIRRef(Fragment *, IRRef1 ref);

void printPrettyIR(Fragment *F, int fragment_id);
void printPrettyIRRef_(Fragment *F, IRRef ref, int follow);
void printPrettyIRIns(Fragment *F, IRRef ref);

INLINE_HEADER void
printPrettyIRRef(Fragment *F, IRRef ref)
{
  printPrettyIRRef_(F, ref, 1);
}



#endif
