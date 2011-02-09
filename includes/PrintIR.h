#ifndef _LAMBDACHINE_PRINT_IR_H
#define _LAMBDACHINE_PRINT_IR_H

#include "IR.h"
#include "Jit.h"

void printIR(JitState *J, IRIns ir);
void printIRRef(JitState *J, IRRef1 ref);

#endif
