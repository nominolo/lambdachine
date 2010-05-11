#ifndef LC_INFOTABLES_H
#define LC_INFOTABLES_H

#include "def.h"
#include "bc.h"

/*
 * A large bitmap.
 */
typedef struct {
  StgWord size;
  StgWord bitmap[FLEXIBLE_ARRAY];
} LcLargeBitmap;

/* A thing that (normally) describes the closure layout.
   size = one word.
*/
typedef union {
  struct {
    StgHalfWord ptrs;  // number of pointers
    StgHalfWord nptrs; // number of non-pointers
  } payload;
  
  StgWord bitmap; // bit map that describes pointers and non-pointers.

  LcLargeBitmap *large_bitmap;  // if we need more than 32/64 bits.

  // possibly add more stuff here.
  
} LcClosureInfo;

/* The common part of any info table.

   The `type` field defines which other information is present.
*/
typedef struct LcInfoTable_ {
  char* closure_type;
  char* closure_desc;

  LcClosureInfo layout; // closure layout info (one word)
  
  StgHalfWord type;       // closure type
  StgHalfWord srt_bitmap; // number of entries in SRT or constructor tag
  
  BcIns code[];
} *LcInfoTablePtr;

typedef struct LcConInfoTable_ {
  char *con_descr;  // name of the data constructor in the form:
		    //     Package:Module.Name
  LcInfoTable i;
} LcConInfoTable;








#endif
