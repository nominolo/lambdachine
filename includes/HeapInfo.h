#ifndef _LAMBDACHINE_HEAPINFO_H
#define _LAMBDACHINE_HEAPINFO_H

#include "Jit.h"

#define JIT_MAXALLOCS   200

void growHeapInfoBuffer_(JitState *J, Word needed);
void growHeapInfoMapBuffer_(JitState *J, Word needed);
u4 newHeapInfo(JitState *J, IRRef1 ref, InfoTable *info);
u4 cloneHeapInfo(JitState *J, IRRef1 ref, u2 orig);
HeapInfo *getHeapInfo(JitState *J, IRRef ref);
void printHeapInfo(JitState *J);
void heapSCCs(JitState *J);


INLINE_HEADER void growHeapInfoBuffer(JitState *J, Word needed)
{
  if (LC_UNLIKELY(needed > J->sizeheap))
    growHeapInfoBuffer_(J, needed);
}

INLINE_HEADER void growHeapInfoMapBuffer(JitState *J, Word needed)
{
  if (LC_UNLIKELY(needed > J->sizeheapmap))
    growHeapInfoMapBuffer_(J, needed);
}

INLINE_HEADER IRRef
getHeapInfoField(JitState *J, HeapInfo *hp, u4 field)
{
  HeapEntry *he = &J->cur.heapmap[hp->mapofs + (field >> 1)];
  if (field & 1)
    return (IRRef)(u2)(*he >> 16);
  else
    return (IRRef)(u2)(*he & 0xffff);
}

INLINE_HEADER void
setHeapInfoField(JitState *J, HeapInfo *hp, u4 field, TRef tr)
{
  //  printf("")
  HeapEntry *he = &J->cur.heapmap[hp->mapofs + (field >> 1)];
  if (field & 1) // set high part
    *he = (*he & 0xffff) | ((HeapEntry)tref_ref(tr) << 16);
  else
    *he = (*he & 0xffff0000) | ((HeapEntry)tref_ref(tr));
}


#endif
