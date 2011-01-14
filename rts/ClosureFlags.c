/* Defines closure flags.  See InfoTables.h */

#import "InfoTables.h"

u2 closure_flags[] = {
#define DEF_CLOS_FLAGS(name, flags)  [name] = CF_##flags,
  CTDEF(DEF_CLOS_FLAGS)
#undef DEF_CLOS_FLAGS
};

