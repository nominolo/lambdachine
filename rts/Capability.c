#include "Capability.h"
#include "MiscClosures.h"
#include "Jit.h"
#include "Stats.h"

#include <stdlib.h>

Capability *G_cap0;

void
initVM()
{
  int i;
  G_cap0 = xmalloc(sizeof(Capability));

  initialiseCapability(G_cap0);

  for (i = -128; i < 128; i++) {
    smallInt(i).info = &stg_Izh_con_info;
    smallInt(i).val = i;
  }

  initMiscClosures();
  initEvents();
}

void
initialiseCapability(Capability *cap)
{
  int i;

  cap->flags = 0;

#if LC_HAS_JIT
  // Initialise hot counters.
  for (i = 0; i < HOTCOUNT_SIZE; i++)
    cap->hotcount[i] = HOTCOUNT_DEFAULT;

  initJitState(&cap->J);
#endif
}

void*
allocate(Capability *cap, u4 num_words)
{
  return xmalloc(num_words * sizeof(Word));
}
