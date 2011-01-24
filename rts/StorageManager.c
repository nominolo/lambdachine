#include "StorageManager.h"
#include "MiscClosures.h"

#include <stdlib.h>
#include <stdio.h>

typedef struct _MemRegion {
  Word *start;
  Word *next;
  Word *top;
  u4 nwords;
  struct _MemRegion *next_region;
} MemRegion;

typedef struct _StorageManagerState {
  MemRegion *infoTables;
  MemRegion *staticClosures;
  MemRegion *closures;
} StorageManagerState;

StorageManagerState G_storage;

MemRegion *allocRegion(u4 nwords);
void *allocInRegion(MemRegion **rp, u4 nwords);
int isInRegion(MemRegion *r, void *p);

#define REGION_SIZE   ((1024 * 1024) / sizeof(Word))

void
initStorageManager()
{
  G_storage.infoTables = allocRegion(REGION_SIZE);
  G_storage.staticClosures = allocRegion(REGION_SIZE);
  G_storage.closures = allocRegion(REGION_SIZE);
}

void* allocInfoTable(u4 nwords)
{
  return allocInRegion(&G_storage.infoTables, nwords);
}

void* allocStaticClosure(u4 nwords)
{
  return allocInRegion(&G_storage.staticClosures, nwords);
}

void* allocClosure(u4 nwords)
{
  return allocInRegion(&G_storage.closures, nwords);
}

int looksLikeInfoTable(void *p)
{
  return isInRegion(G_storage.infoTables, p)
    || p == &stg_IND_info;
}

int looksLikeClosure(void *p)
{
  return isInRegion(G_storage.staticClosures, p)
    || isInRegion(G_storage.closures, p)
    || p == &stg_UPD_closure;
}

int isClosure(void *p)
{
  return looksLikeClosure(p)
    && looksLikeInfoTable((void*)getInfo((Closure*)p));
}

void *xmalloc(size_t bytes) {
  void *p = malloc(bytes);
  if (p == NULL) {
    fprintf(stderr, "FATAL: Out of memory.\n");
    exit(2);
  }
  return p;
}

MemRegion *
allocRegion(u4 nwords)
{
  MemRegion *r = xmalloc(sizeof(MemRegion));

  r->start = xmalloc(nwords * sizeof(Word));
  r->next = r->start;
  r->top = r->start + nwords;
  r->nwords = nwords;
  r->next_region = NULL;

  return r;
}

void *
allocInRegion(MemRegion **rp, u4 nwords)
{
  MemRegion *r = *rp;
  LC_ASSERT(nwords > 0 && nwords <= REGION_SIZE);
  // Round to even number of words;
  if (nwords & 1) nwords ++;
  if (r->next + nwords <= r->top) {
    void *res = r->next;
    r->next += nwords;
    return res;
  } else {
    MemRegion *s = allocRegion(REGION_SIZE);
    void *res = s->top;
    s->next_region = r;
    *rp = s;
    s->top += nwords;
    return res;
  }
}

int
isInRegion(MemRegion *r, void *p)
{
  Word* q = (Word*)p;
  while (r != NULL) {
    if (q >= r->start && q <= r->top)
      return 1;
    r = r->next_region;
  }
  return 0;
}
