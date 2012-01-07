/*
** IR assembler (SSA IR -> machine code).
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LAMBDACHINE_ASMCODEGEN_H
#define _LAMBDACHINE_ASMCODEGEN_H

#include "Common.h"
#include "Jit.h"

void genAsm(JitState *J, Fragment *T);
void dumpAsm(MCode* mcode, MSize sz, FILE* out);

#endif
