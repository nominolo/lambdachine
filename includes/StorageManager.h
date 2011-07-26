#ifndef _LAMBDACHINE_STORAGE_MANAGER_H
#define _LAMBDACHINE_STORAGE_MANAGER_H

#include "Common.h"
#include "VM.h"
#include "Bytecode.h"

#define BLOCK_SHIFT             15
#define BLOCK_SIZE              (1ul << BLOCK_SHIFT)
#define BLOCK_MASK              (BLOCK_SIZE - 1)

// Block Flags
#define BF_UNINITIALIZED        0x0000
#define BF_CLOSURES             0x0001
#define BF_INFO_TABLES          0x0002
#define BF_STATIC_CLOSURES      0x0003
#define BF_CONTENTS_MASK        0x000f
#define BF_EVACUATED            0x0010

#define ROUND_UP_TO_BLOCK_SIZE(x) \
  (((Word)(x) + BLOCK_SIZE - 1) & ~(BLOCK_SIZE - 1))

#define BLOCK_ALIGNED(x) \
  (((Word)(x) & (BLOCK_SIZE - 1)) == 0)

typedef struct _BlockDescr {
  char               *start;    /* Start of allocatable data. */
  char               *free;     /* Next free byte */
  struct _BlockDescr *link;     /* Link to next block. */
  u2                  flags;
  u2                  unused;
#if LC_ARCH_BITS == 64
  u4                  pad;
#endif
} BlockDescr;




// Tests whether x's size is a multiple of the size of a word.
// The magic number "3" is there because 1 byte = 2^3 bits.
#define WORD_ALIGNED_SIZE(x) \
  ((sizeof(x) & ((1 << LC_ARCH_BYTES_LOG2) - 1)) == 0)

LC_STATIC_ASSERT(WORD_ALIGNED_SIZE(BlockDescr));

/* For debug output. */
typedef struct _RegionInfo {
  void               *start, *end;
  struct _RegionInfo *next;
} RegionInfo;

/* Memory manager state */
typedef struct _StorageManagerState {
  BlockDescr *empty;		/* Linked list of empty blocks */
  BlockDescr *full;		/* Linked list of full blocks */
  BlockDescr *current;     	/* Pointer to current closure
				   allocation block */
  BlockDescr *infoTables;	/* Linked list of info table blocks */
  BlockDescr *staticClosures;	/* Linked list of static closures */
  BlockDescr *scav_todo;

  /* State concerning the dynamic region of the heap. */
  u4 nfull;                     /* Number of full blocks. */
  u4 ntotal;                    /* Total number of blocks (full and empty) */
  u4 nextgc;                    /* Do next GC when nfull reaches this
                                   value. */
  u4 gc_inprogress;             /* 1 iff GC is in progress, 0 otherwise */
  Word *hp;                     /* Next location to allocate */
  Word *limit;                  /* Upper bound for current allocation
                                   buffer. */
  u8 allocated;                 /* Words allocated */

  /* Debug info. */
  RegionInfo *regions;
} StorageManagerState;

static inline BlockDescr *bDescr(void *p)
{
  return (BlockDescr*)((Word)p & ~BLOCK_MASK);
}


void initStorageManager();
void dumpStorageManagerState();

void* allocInfoTable(u4 nwords);
void* allocStaticClosure(u4 nwords);
void* allocClosure(u4 nwords);
void* allocClosure_(u4 nwords, Thread *T, BCIns *pc, Word *base);
void *allocClosureDuringGC(u4 nwords);
char *allocString(u4 len);
int looksLikeInfoTable(void *);
int looksLikeClosure(void *);
int looksLikeStaticClosure(void *);
int isClosure(void *p);

void performGC(Capability *C);

void makeCurrent(StorageManagerState *M, BlockDescr *blk);
BlockDescr *getEmptyBlock(StorageManagerState *M);

#endif
