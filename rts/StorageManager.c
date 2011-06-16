#include "StorageManager.h"
#include "MiscClosures.h"

#include <sys/mman.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>


#define BLOCK_SIZE              (1ul << 15)

// Block Flags
#define BF_UNINITIALIZED        0x0000
#define BF_CLOSURES             0x0001
#define BF_INFO_TABLES          0x0002
#define BF_STATIC_CLOSURES      0x0003
#define BF_CONTENTS_MASK        0x000f

#define ROUND_UP_TO_BLOCK_SIZE(x) \
  (((Word)(x) + BLOCK_SIZE - 1) & ~(BLOCK_SIZE - 1))

#define BLOCK_ALIGNED(x) \
  (((Word)(x) & (BLOCK_SIZE - 1)) == 0)

/* mmap-related stuff */

#define MMAP_PROT		(PROT_READ|PROT_WRITE)

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
# define MAP_ANONYMOUS          MAP_ANON
#endif

#define MMAP_FLAGS              (MAP_PRIVATE|MAP_ANONYMOUS)
#define MMAP_REGION_START       ((char*)0x10000)
#if LC_ARCH_BITS == 32
# define MMAP_REGION_END        ((char*)0x80000000)
#elif LC_ARCH_BITS == 64
# define MMAP_REGION_END        ((char*)(1ul << 40))
#endif

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
#define WORD_ALIGNED_SIZE(x) \
  ((sizeof(x) & ((1 << (LC_ARCH_BITS_LOG2 - 3)) - 1)) == 0)

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

  /* State concerning the dynamic region of the heap. */
  u4 nfull;                     /* Number of full blocks. */
  u4 ntotal;                    /* Total number of blocks (full and empty) */
  u4 nextgc;                    /* Do next GC when nfull reaches this
                                   value. */
  Word *hp;                     /* Next location to allocate */
  Word *limit;                  /* Upper bound for current allocation
                                   buffer. */
  u8 allocated;                 /* Words allocated */

  /* Debug info. */
  RegionInfo *regions;
} StorageManagerState;

StorageManagerState G_storage;

void initStorageManager();
void makeCurrent(StorageManagerState *M, BlockDescr *blk);
BlockDescr *allocBlock(StorageManagerState *M);
void addInfoTableBlock(StorageManagerState *M);
void addStaticClosuresBlock(StorageManagerState *M);
void outOfMemory();

void
initStorageManager()
{
  G_storage.empty = NULL;
  G_storage.full  = NULL;
  G_storage.current = NULL;
  G_storage.infoTables = NULL;
  G_storage.staticClosures = NULL;

  G_storage.nfull = 0;
  G_storage.ntotal = 0;
  G_storage.nextgc = 32;
  G_storage.hp = NULL;
  G_storage.limit = NULL;
  G_storage.allocated = 0;

  RegionInfo *ri = xmalloc(sizeof(RegionInfo));  // TODO: Don't use malloc
  ri->start = NULL;
  ri->end   = NULL;
  ri->next  = NULL;
  G_storage.regions = ri;

  makeCurrent(&G_storage, allocBlock(&G_storage));
  addInfoTableBlock(&G_storage);
  addStaticClosuresBlock(&G_storage);
}

BlockDescr *initBlock(void *ptr);

/*
Allocate a new block and push it to the front of the free list.

NOT thread safe!
*/
BlockDescr *
allocBlock(StorageManagerState *M)
{
  // Where to allocate next
  static char *alloc_hint = (char*)ROUND_UP_TO_BLOCK_SIZE(MMAP_REGION_START); 
  size_t size = BLOCK_SIZE;
  char *ptr;
  // int retry = 0;

  for (;;) {
    ptr = mmap(alloc_hint, size, MMAP_PROT, MMAP_FLAGS, -1, 0);
    if (ptr != MAP_FAILED && BLOCK_ALIGNED(ptr)) {
      // SUCCESS
      alloc_hint += size;
      break;
    }
    if (ptr == MAP_FAILED) {
      munmap(ptr, size);
      if (alloc_hint >= MMAP_REGION_END) outOfMemory();
    }
    alloc_hint += size;
  }

  printf("> Allocated new block: %p-%p\n", ptr, ptr + size);

  if (M->regions->start == NULL) {
    M->regions->start = ptr;
  }
  if (M->regions->end == NULL) {
    M->regions->end = ptr + size;
  } else if (M->regions->end == ptr) { // Expand current region
    M->regions->end = ptr + size;
  } else {
    RegionInfo *ri = xmalloc(sizeof(RegionInfo));  // TODO: Don't use malloc
    ri->next = M->regions;
    ri->start = ptr;
    ri->end = ptr + size;
    M->regions = ri;
  }
 
  BlockDescr *blk = initBlock(ptr);

  M->ntotal++;

  return blk;
}



BlockDescr *
initBlock(void *ptr)
{
  BlockDescr *blk = (BlockDescr*)ptr;
  blk->start = ptr + sizeof(BlockDescr);
  blk->free = blk->start;
  blk->link = NULL;
  blk->flags = 0;

  return blk;
}

void
makeCurrent(StorageManagerState *M, BlockDescr *blk)
{
  blk->link = NULL;
  blk->flags = (blk->flags & ~BF_CONTENTS_MASK) | BF_CLOSURES;
  M->current = blk;
  M->hp = (Word*)blk->free;
  M->limit = (Word*)((char*)blk + BLOCK_SIZE);
}

/* Unlink empty block or allocate a new one. */
BlockDescr *
getEmptyBlock(StorageManagerState *M)
{
  BlockDescr *blk;
  if (M->empty != NULL) {
    blk = M->empty;
    M->empty = blk->link;
    blk->link = NULL;
    return blk;
  } else {
    return allocBlock(M);
  }
}

void
currentBlockFull(StorageManagerState *M)
{
  LC_ASSERT(M->current);
  BlockDescr *blk = M->current;
  M->current = NULL;

  // Accounting stuff
  M->allocated += M->hp - (Word*)blk->free;
  blk->free = (char*)M->hp;

  // Link into full list
  blk->link = M->full;
  M->full = blk;
  M->nfull++;

  if (M->nfull >= M->nextgc) {
    printf("TODO: PerformGC\n");
  }

  makeCurrent(M, getEmptyBlock(M));
}

void
addInfoTableBlock(StorageManagerState *M)
{
  BlockDescr *blk = allocBlock(M);
  blk->flags |= BF_INFO_TABLES;
  blk->link = M->infoTables;
  M->infoTables = blk;
}

void
addStaticClosuresBlock(StorageManagerState *M)
{
  BlockDescr *blk = allocBlock(M);
  blk->flags |= BF_STATIC_CLOSURES;
  blk->link = M->staticClosures;
  M->staticClosures = blk;
}

void* allocClosure(u4 nwords)
{
  // printf(">> allocClosure(%d)\n", nwords);
 retry:
  if (LC_LIKELY(G_storage.hp + nwords <= G_storage.limit)) {
    void *p = G_storage.hp;
    G_storage.hp += nwords;
    //printf(">> Allocated closure: %p-%p (%u)\n",
    //	   p, G_storage.hp, nwords);
    return p;
  }

  currentBlockFull(&G_storage);
  goto retry;
}

void*
allocInfoTable(u4 nwords)
{
  BlockDescr *blk;
 retry:
  blk = G_storage.infoTables;
  if (LC_LIKELY(blk->free + nwords * sizeof(Word)
      <= (char*)blk + BLOCK_SIZE)) {
    void *p = blk->free;
    blk->free += nwords * sizeof(Word);
    // printf(">> Allocated info table: %p-%p (%u)\n",
    //   p, blk->free, nwords);
    return p;
  }
  addInfoTableBlock(&G_storage);
  goto retry;
}

void* allocStaticClosure(u4 nwords)
{
  BlockDescr *blk;
 retry:
  blk = G_storage.staticClosures;
  if (LC_LIKELY(blk->free + nwords * sizeof(Word)
      <= (char*)blk + BLOCK_SIZE)) {
    void *p = blk->free;
    blk->free += nwords * sizeof(Word);
    //printf(">> Allocated static closure: %p-%p (%u)\n",
    //	   p, blk->free, nwords);
    return p;
  }
  addStaticClosuresBlock(&G_storage);
  goto retry;
}

int
isManagedMemory(void *p)
{
  RegionInfo *ri = G_storage.regions;
  while (ri) {
    if (ri->start <= p && p < ri->end)
      return 1;
    ri = ri->next;
  }
  return 0;
}

#define PTR_TO_BLOCK_DESCR(p) \
  ((BlockDescr*)((Word)(p) & ~(BLOCK_SIZE - 1)))

int looksLikeInfoTable(void *p) {
  return (isManagedMemory(p) &&
          (PTR_TO_BLOCK_DESCR(p)->flags & BF_CONTENTS_MASK) ==
	  BF_INFO_TABLES)
    || p == &stg_IND_info;;
}

int looksLikeClosure(void *p)
{
  return (isManagedMemory(p) &&
          (PTR_TO_BLOCK_DESCR(p)->flags & BF_CONTENTS_MASK) ==
	  BF_CLOSURES)
    || looksLikeStaticClosure(p)
    || p == &stg_UPD_closure;
}

int looksLikeStaticClosure(void *p)
{
  return (isManagedMemory(p) &&
          (PTR_TO_BLOCK_DESCR(p)->flags & BF_CONTENTS_MASK) ==
	  BF_STATIC_CLOSURES);
}

int isClosure(void *p)
{
  return looksLikeClosure(p)
    && looksLikeInfoTable((void*)getInfo((Closure*)p));
}

void
formatWithThousands(char *str, u8 n)
{
  u4 ns[7];
  int i = 0;
  char *p = str;
  do {
    ns[i] = n % 1000;
    n = n / 1000;
    i++;
  } while (n > 0);
  i--;
  p += sprintf(p, "%3d", ns[i]);
  for (i = i - 1; i >= 0; i--) {
    p += sprintf(p, ",%03d", ns[i]);
  }
}

void
dumpStorageManagerState()
{
  StorageManagerState *M = &G_storage;
  /* u4 freeblocks = 0; */
  /* Word freemem = 0; */
  BlockDescr *d;
  char str[30];

  d = M->current;
  formatWithThousands(str, M->allocated * (LC_ARCH_BITS / 8) +
         ((char*)M->hp - d->free));
  printf("**********************************************\n"
         "Bytes allocated:    %20s bytes\n", str);

  printf("Alloc:       hp=%p, limit=%p\n", M->hp, M->limit);
  printf("Blocks:      %u full, %u total\n", M->nfull, M->ntotal);

  u8 block_capacity = ((char*)d + BLOCK_SIZE) - d->start;
  u8 block_full = (char*)M->hp - d->start;

  printf("Current:     %p-%p, %" FMT_Word64 "%% full\n",
         d->start, (char*)d + BLOCK_SIZE,
         (100 * block_full) / block_capacity );

  /*
  for (d = M->empty; d != NULL; d = d->link) {
    printf("Empty block: %p-%p\n",  d->start, (char*)d + BLOCK_SIZE);
  }
  for (d = M->full; d != NULL; d = d->link) {
    printf("Full block:  %p-%p\n",  d->start, (char*)d + BLOCK_SIZE);
  }
  */
}

void
outOfMemory()
{
  fprintf(stderr, "FATAL: Out of memory\n");
  exit(1);
}
