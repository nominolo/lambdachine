#ifndef _LAMBDACHINE_THREAD_H
#define _LAMBDACHINE_THREAD_H

#include "Capability.h"
#include "Common.h"
#include "Bytecode.h"
#include "InfoTables.h"

typedef struct Thread_ {
  Word       header;
  BCIns      *pc;
  u4         stack_size;        /* Stack size in _words_ */
  Word       *base;             /* The current base pointer. */
  Word       *top;              /* Top of stack */
  Word       stack[FLEXIBLE_ARRAY];
} Thread;

Thread *createThread(Capability *cap, u4 stack_size);

int stackOverflow(Thread *, Word *top, u4 increment);

Closure *startThread(Thread *, Closure *);

#define THREAD_STRUCT_SIZE  (sizeof(Thread))

#define THREAD_STRUCT_SIZEW (THREAD_STRUCT_SIZE / sizeof(Word))

#endif
