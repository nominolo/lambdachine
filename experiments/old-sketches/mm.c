#include <stdlib.h>
#include <assert.h>

#include "mm.h"

/**
 * Initialise the memory manager state.
 *
 * Currently we use a very simple semi-space garbage collector.  The
 * heap is split into two equally sized regions and objects are
 * bump-allocated into one region.  If the region is full, garbage
 * collection occurs by coping all live objects into the other region
 * and (implicitly) throw away all the remaining objects.  The two
 * regions then switch roles.
 *
 * @param heap_size The heap size in bytes.  This value be must at
 * least 1024 machine words.
 *
 * @return 0 if no error occurred.  #ENOMEM if allocation of the heap
 * failed.
 */
int MMState_init(MMState* state, StgWord heap_size)
{
  StgPtr heap;
  StgWord space_size;
  
  assert(heap_size >= 1024 * sizeof(StgWord));

  // heap_size should be a multiple of sizeof(StgWord)
  heap_size = heap_size & ~((1 << LC_ALIGN_LOG2) - 1);
  
  heap = (StgPtr)malloc(heap_size);
  if (!heap)
    return ENOMEM;

  space_size = heap_size >> 1;
  
  state->heap_bottom = heap;
  state->heap_size = heap_size;
  state->Hp = heap;
  state->HpLim = heap + space_size;
  state->from_space = heap + space_size + sizeof(StgWord);
  state->root = heap;

  return 0;
}

// obj_size is in *machine words*!
StgPtr new(MMState* state, StgWord obj_size)
{
  if (state->Hp + obj_size > state->HpLim) {
    flip(state);
    if (state->Hp + obj_size > state->HpLim) {
      // out of memory
      exit(1);
    }
  }
  state->Hp += obj_size;
  return state->Hp - obj_size;
}

int flip(MMState* state)
{
  // Just die when out of memory (for now)
  return 0;
  /*
  StgPtr old_Hp;
  
  old_Hp = state->Hp;
  state->Hp = state->from_space;
  state->from_space = old_Hp;

  evac(state, state->root);
  
  return 0;
  */
}

#define CONSTR                  1
#define CONSTR_1_0              2
#define CONSTR_0_1              3
#define CONSTR_2_0              4
#define CONSTR_1_1              5
#define CONSTR_0_2              6
#define CONSTR_STATIC	        7

int evac(MMState* state, StgPtr object)
{
  unsigned i;
  unsigned size = 1; //OBJ_SIZE(object);
  StgPtr new_object = state->Hp;

  // allocate memory
  state->Hp += 1 + size;

  // copy info table pointer
  new_object[0] = object[0];

  // Overwrite the old info table with a forwarding pointer.
  // This 
  object[0] = (StgPtr)((StgWord)new_object | 1);
  
  // copy object
  for (i = 1; i <= size; ++i) {
    new_object[i] = object[i];
  }

  // 
  object[0] = (StgWord)new_object;
}
