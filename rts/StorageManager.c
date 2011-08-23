#include "StorageManager.h"
#include "MiscClosures.h"
#include "Capability.h"
#include "Thread.h"

#include <sys/mman.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>


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


StorageManagerState G_storage;

void initStorageManager();
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
  G_storage.nextgc = 2;
  G_storage.gc_inprogress = 0;
  G_storage.hp = NULL;
  G_storage.limit = NULL;
  G_storage.allocated = 0;
  G_storage.copied = 0;

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

  DBG_LVL(1, "> Allocated new block: %p-%p\n", ptr, ptr + size);

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
  M->limit = (Word*)BLOCK_END(blk);
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

  if (M->nfull >= M->nextgc && !M->gc_inprogress) {
    performGC(G_cap0);
    //    printf("TODO: PerformGC\n");
  } else {
    makeCurrent(M, getEmptyBlock(M));
  }
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

// Used by the interpreter.  Ensures thread state is in sync with
// interpreter state when GC is triggered.
//
// pc points *after* the allocation instruction
void* allocClosure_(u4 nwords, Thread *T, BCIns *pc, Word *base)
{
  if (nwords == 1) nwords = 2;  // Silly hack
  // printf(">> allocClosure(%d)\n", nwords);
 retry:
  if (LC_LIKELY(G_storage.hp + nwords <= G_storage.limit)) {
    void *p = G_storage.hp;
    G_storage.hp += nwords;
    //printf(">> Allocated closure: %p-%p (%u)\n",
    //	   p, G_storage.hp, nwords);
    DBG_PR("allocClosure_(%d,...) => %p\n", nwords, p);
    return p;
  }

  // state in T must be up to date because we may have to do GC
  T->pc = pc;
  T->base = base;
  currentBlockFull(&G_storage);
  goto retry;

}

void* allocClosure(u4 nwords)
{
  if (nwords == 1) nwords = 2;  // Silly hack
  // printf(">> allocClosure(%d)\n", nwords);
 retry:
  if (LC_LIKELY(G_storage.hp + nwords <= G_storage.limit)) {
    void *p = G_storage.hp;
    G_storage.hp += nwords;
    //printf(">> Allocated closure: %p-%p (%u)\n",
    //	   p, G_storage.hp, nwords);
    DBG_PR("allocClosure(%d) => %p\n", nwords, p);
    return p;
  }

  currentBlockFull(&G_storage);
  goto retry;
}

void *allocClosureDuringGC(u4 nwords)
{
  if (nwords == 1) nwords = 2;
 retry:
  if (LC_LIKELY((Word*)G_storage.current->free + nwords
                <= (Word*)((char*)G_storage.current + BLOCK_SIZE))) {
    void *p = G_storage.current->free;
    G_storage.current->free += nwords * sizeof(Word);
    G_storage.copied += nwords;
    //DBG_PR("allocClosureDuringGC(%d) => %p\n", nwords, p);
    return p;
  }
  
  // Block full
  BlockDescr *blk = G_storage.current;
  G_storage.current = NULL;

  // All GC'd blocks have been unlinked from the full list, so it's
  // fine to link them here.
  blk->link = G_storage.full;
  G_storage.full = blk;
  G_storage.nfull++;

  makeCurrent(&G_storage, getEmptyBlock(&G_storage));
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
  if (nwords == 1) nwords = 2; // Silly hack
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
  printf("************************************************************\n"
         "Bytes allocated:                  %20s bytes\n", str);
  formatWithThousands(str, M->copied * (LC_ARCH_BITS / 8));
  printf("Bytes copied:                     %20s bytes\n", str);

  printf("Alloc:            hp=%p, limit=%p\n", M->hp, M->limit);
  printf("Blocks:           %u full, %u total, next GC: %u full blocks\n",
         M->nfull, M->ntotal, M->nextgc);

  u8 block_capacity = ((char*)d + BLOCK_SIZE) - d->start;
  u8 block_full = (char*)M->hp - d->start;

  printf("Current:          %p-%p, %" FMT_Word64 "%% full, flags = %x\n",
         d->start, BLOCK_END(d),
         (100 * block_full) / block_capacity,
         d->flags);

  // for (d = M->empty; d != NULL; d = d->link) {
  //   printf("Empty block: %p-%p\n",  d->start, (char*)d + BLOCK_SIZE);
  // }
  for (d = M->full; d != NULL; d = d->link) {
    printf("Full block:  %p-%p\n",  d->start, BLOCK_END(d));
  }
  for (d = M->infoTables; d != NULL; d = d->link) {
    printf("Itbl block:  %p-%p\n",  d->start, BLOCK_END(d));
  }
  for (d = M->staticClosures; d != NULL; d = d->link) {
    printf("Static block:  %p-%p\n",  d->start, BLOCK_END(d));
  }
}

void
outOfMemory()
{
  fprintf(stderr, "FATAL: Out of memory\n");
  exit(1);
}

char *
allocString(u4 len)
{
  // TODO: Alloc into special block?
  return xmalloc(len + 1);
}
