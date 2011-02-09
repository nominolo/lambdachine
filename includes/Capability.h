#ifndef _LAMBDACHINE_CAPABILITY_H
#define _LAMBDACHINE_CAPABILITY_H

#include "Common.h"
#include "VM.h"

#include "Jit.h"

typedef int AsmFunction;

#define DISPATCH_TABLE_LEN   64

/* The VM state associated with an OS thread. */
struct Capability_ {
  Thread   *T;                 /* Currently running thread. */
#if LC_HAS_JIT
  JitState  J;
  HotCount  hotcount[HOTCOUNT_SIZE]; /* Hot counters. */
#endif
};

extern Capability* G_cap0;

#define hotcount_get(cap, pc) \
  (cap)->hotcount[(u4ptr(pc) >> 2) & (HOTCOUNT_SIZE - 1)]
#define hotcount_set(J, pc, val) \
  (hotcount_get((cap), (pc)) = (HotCount)(val))

INLINE_HEADER int
hotcountTick(Capability *cap, const BCIns *pc, Word *base)
{
  JitState *J = &cap->J;
  if (LC_UNLIKELY(J->mode != 0))
    return 0;

  HotCount c = --cap->hotcount[(u4ptr(pc) >> 2) & (HOTCOUNT_SIZE - 1)];
  if (LC_UNLIKELY(c == 0)) {
    // Target has become hot.

    // Reset hotcount
    hotcount_set(cap, pc, HOTCOUNT_DEFAULT);
    startRecording(J, pc, cap->T, base);
    return 1;
  }

  return 0;
}


void *allocate(Capability *cap, u4 num_words);

void initVM();
void initialiseCapability(Capability *cap);

#endif
