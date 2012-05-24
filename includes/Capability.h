#ifndef _LAMBDACHINE_CAPABILITY_H
#define _LAMBDACHINE_CAPABILITY_H

#include "Common.h"
#include "VM.h"

#include "Jit.h"
#include "Opts.h"
#include "Stats.h"

typedef void *AsmFunction;

#define DISPATCH_TABLE_LEN   64

typedef enum {
  CF_NO_JIT = 1,
  CF_SINGLE_STEP = 2,
} CapabilityFlag;

/* The VM state associated with an OS thread. */
struct Capability_ {
  Thread   *T;                 /* Currently running thread. */
  u4        flags;
  Closure  *static_objs;        /* A linked list of updated static
                                   objects.  These must be GC roots. */
#if LC_HAS_JIT
  JitState  J;
  HotCount  hotcount[HOTCOUNT_SIZE]; /* Hot counters. */
  Word      step_result;             /* Saved code pointer.  For
                                        verification mode only. */
#endif

  const AsmFunction *dispatch;        /* The current dispatch table. */
  /* Pointers to the dispatch tables for various modes. */
  const AsmFunction *dispatch_normal;
  const AsmFunction *dispatch_record;
  const AsmFunction *dispatch_single_step;
  /* If this special label is called, the state is reloaded from the
     capability. */
  BCIns *reload_state_pc;
};

extern Capability* G_cap0;

#define hotcount_hash(pc) \
  (((u4ptr(pc)>>12) ^ (u4ptr(pc)>>4)) & (HOTCOUNT_SIZE - 1))

#define hotcount_get(cap, pc) \
  (cap)->hotcount[hotcount_hash(pc)]
#define hotcount_set(cap, pc, val) \
  (hotcount_get((cap), (pc)) = (HotCount)(val))

void *allocate(Capability *cap, u4 num_words);

void initVM(const Opts* opts);
void initialiseCapability(Capability *cap, const Opts* opts);

/**
 * Increments the hot counter for the given PC.
 *
 * @return true iff the counter has exceeded the hotness threshold.
 */
static inline bool
incrementHotCounter(Capability *cap, JitState *J, const BCIns *pc)
{
  recordEvent(EV_TICK, 0);

  // Ignore if we're already recording.
  if (LC_UNLIKELY(J->mode != JIT_MODE_NORMAL)) {
    return false;
  }

  HotCount c = --cap->hotcount[hotcount_hash(pc)];
  DBG_PR("HOT_TICK: [%d] = %d\n", hotcount_hash(pc), c);
  if (LC_UNLIKELY(c == 0)) {   // Target has become hot.
    hotcount_set(cap, pc, HOTCOUNT_DEFAULT); // Reset hotcount
    return true;
  }
  return false;
}



#endif
