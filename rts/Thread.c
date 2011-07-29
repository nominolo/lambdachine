#include "Thread.h"
#include "Capability.h"
#include "MiscClosures.h"

#define MIN_STACK_WORDS 64

Thread *
createThread(Capability *cap, u4 size)
{
  Thread *T;
  u4 stack_size;
  Word i;

  if (size < MIN_STACK_WORDS + THREAD_STRUCT_SIZEW) {
    size = MIN_STACK_WORDS + THREAD_STRUCT_SIZEW;
  }

  // TODO: maybe round size to mem manager block size
  T = (Thread *)allocate(cap, size);
  stack_size = size - THREAD_STRUCT_SIZEW;

  for (i = 0; i < stack_size; i++) { // helpful for debugging
    T->stack[i] = 0xfbad0000 + i;
  }

  T->header = 42;  // TODO
  T->stack_size = stack_size;
  T->stack[0] = (Word)NULL;  // previous base
  T->stack[1] = (Word)NULL;  // previous PC
  T->stack[2] = (Word)&stg_STOP_closure;
  T->stack[3] = (Word)NULL;
  T->base = &T->stack[3];
  T->top = &T->stack[4];
  T->pc = getFInfo(&stg_STOP_closure)->code.code;

  return T;
}
