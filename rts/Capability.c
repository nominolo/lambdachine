#include "Capability.h"
#include "MiscClosures.h"
#include "Jit.h"
#include "Stats.h"
#include "Opts.h"
#include "Interp.h"

#include <stdlib.h>

Capability *G_cap0;

void
initVM(const Opts* opts)
{
  int i;
  G_cap0 = xmalloc(sizeof(Capability));

#ifdef LC_SELF_CHECK_MODE
  initShadowHeap();
#endif

  initialiseCapability(G_cap0, opts);

  for (i = -128; i < 128; i++) {
    smallInt(i).info = &stg_Izh_con_info;
    smallInt(i).val = i;
  }

  initMiscClosures();
  initEvents();
}

void
initialiseCapability(Capability *cap, const Opts* opts)
{
  int i;

  cap->flags = 0;
  cap->static_objs = NULL;
  initDispatchTables(cap);

#if LC_HAS_JIT
  // Initialise hot counters.
  for (i = 0; i < HOTCOUNT_SIZE; i++)
    cap->hotcount[i] = HOTCOUNT_DEFAULT;

  initJitState(&cap->J, opts);
#endif
}

void*
allocate(Capability *cap, u4 num_words)
{
  return xmalloc(num_words * sizeof(Word));
}
