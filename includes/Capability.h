#ifndef LC_CAPABILITY_H
#define LC_CAPABILITY_H

#include "Common.h"

typedef int AsmFunction;

#define DISPATCH_TABLE_LEN   64

/* Type for hot counter. */
typedef u2 HotCount;

/* Number of entries in the hash table for hot counters.
   The hash value is calculated by applying the mask to the target PC.
 */
#define HOTCOUNT_SIZE           64
#define HOTCOUNT_MASK           ((HOTCOUNT_SIZE - 1) * sizeof(HotCount))



/* The VM state associated with an OS thread. */
typedef struct CapabilityState {

  HotCount hotcount[HOTCOUNT_SIZE]; /* Hot counters are per-thread. */
  //AsmFunction dispatch[DISPATCH_TABLE_LEN];
    /* Instruction dispatch table.  Because the dispatch table
       represents the interpreter mode, we use a per-thread dispatch
       table. */
} CapabilityState;

typedef CapabilityState Capability;

extern Capability* cap0;


#define hotcountGet(cap, pc) \
  (cap)->hotcount[(u4ptr(pc) >> 2) & (HOTCOUNT_SIZE - 1)]
#define hotcountSet(cap, pc, val) \
  (hotcountGet((cap), (pc)) = (HotCount)(val))

void *allocate(Capability *cap, u4 num_words);

void initVM();

#endif
