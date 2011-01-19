#include "Capability.h"
#include "MiscClosures.h"

#include <stdlib.h>

Capability *cap0;

void
initVM()
{
  int i;
  cap0 = malloc(sizeof(CapabilityState));
  for (i = 0; i < HOTCOUNT_SIZE; i++)
    cap0->hotcount[i] = 0;

  for (i = -128; i < 128; i++) {
    smallInt(i).info = &stg_Izh_con_info;
    smallInt(i).val = i;
  }

  initAPClosures();
}

void*
allocate(Capability *cap, u4 num_words)
{
  return malloc(num_words * sizeof(Word));
}
