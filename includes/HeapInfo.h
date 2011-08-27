#ifndef _LAMBDACHINE_HEAPINFO_H
#define _LAMBDACHINE_HEAPINFO_H

#include "Jit.h"

#define JIT_MAXALLOCS   200

void growHeapInfoBuffer_(JitState *J, Word needed);
void growHeapInfoMapBuffer_(JitState *J, Word needed);
u4 newHeapInfo(JitState *J, IRRef1 ref, InfoTable *info);
u4 cloneHeapInfo(JitState *J, IRRef1 ref, u2 orig);
HeapInfo *getHeapInfo(JitState *J, IRRef ref);
void printHeapInfo(FILE *file, JitState *J);
void heapSCCs(JitState *J);


INLINE_HEADER void growHeapInfoBuffer(JitState *J, Word needed)
{
  LC_ASSERT(needed > 0);
  if (LC_UNLIKELY(needed > J->sizeheap))
    growHeapInfoBuffer_(J, needed);
}

INLINE_HEADER void growHeapInfoMapBuffer(JitState *J, Word needed)
{
  LC_ASSERT(needed > 0);
  if (LC_UNLIKELY(needed > J->sizeheapmap))
    growHeapInfoMapBuffer_(J, needed);
}

INLINE_HEADER IRRef
getHeapInfoField(Fragment *F, HeapInfo *hp, u4 field)
{
  return (IRRef)F->heapmap[hp->mapofs + field];
}

INLINE_HEADER void
setHeapInfoField(Fragment *F, HeapInfo *hp, u4 field, TRef tr)
{
  F->heapmap[hp->mapofs + field] = (HeapEntry)tref_ref(tr);
}


#endif
