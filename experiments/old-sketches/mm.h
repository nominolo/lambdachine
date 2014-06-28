#include "def.h"

#define ENOMEM 1

typedef struct MMState_ {
  StgPtr  heap_bottom;
  StgWord heap_size;
  StgPtr  Hp;    // The heap pointer
  StgPtr  HpLim; // Top of the heap
  StgPtr  from_space;
  StgPtr  root;
} MMState;

// argument specifies heap size in *bytes*.
int MMState_init(MMState*, StgWord heap_size);
