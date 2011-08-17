#ifndef _LAMBDACHINE_MCODE_H
#define _LAMBDACHINE_MCODE_H

#include "Jit.h"

MCode *reserveMCode(JitState *J, MCode **lim);
void   mcodeCommit(JitState *J, MCode *top);

#define mcodeCommitBot(J, m)	(J->mcbot = (m))

#endif


