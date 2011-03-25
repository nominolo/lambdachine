#ifndef _LAMBDACHINE_PRINT_IR_H
#define _LAMBDACHINE_PRINT_IR_H

#include "IR.h"
#include "Jit.h"

void printIR(Fragment *, IRIns ir);
void printIRRef(Fragment *, IRRef1 ref);

void printPrettyIR(Fragment *F, int fragment_id);

#endif
