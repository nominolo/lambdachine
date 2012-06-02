#include "memorymanager.hh"
#include "utils.hh"

#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

#define DLOG(...) \
  if (DEBUG_COMPONENTS & DEBUG_MEMORY_MANAGER) { \
    fprintf(stderr, "MM: " __VA_ARGS__); }

using namespace lambdachine;

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
# define MAP_ANONYMOUS          MAP_ANON
#endif

#if LC_ARCH_BITS == 64
char *const kMMapRegionStart = reinterpret_cast<char*>(0x100000000);
char *const kMMapRegionEnd = reinterpret_cast<char*>(1UL << 40); // 1 Tbyte
#elif LC_ARCH_BITS == 32
char *const kMMapRegionStart = reinterpret_cast<char*>(0x10000);
char *const kMMapRegionEnd = reinterpret_cast<char*>(1UL << 31); // 2 GB
#else
# error "Only 32 bit and 64 bit architectures supported."
#endif

static void outOfMemory() {
  fprintf(stderr, "FATAL: Out of memory.\n");
  fflush(stderr);
  abort();
}

const int kMMapProtection = PROT_READ | PROT_WRITE;
const int kMMapFlags = MAP_PRIVATE | MAP_ANONYMOUS;

Region *Region::newRegion(RegionType regionType) {
  // TODO: Grab a lock.
  static char *alloc_hint = alignToRegionBoundary(kMMapRegionStart);
  size_t size = kRegionSize;
  char *ptr;

  for (;;) {
    ptr = static_cast<char*>(mmap(alloc_hint, size, kMMapProtection, kMMapFlags, -1, 0));
    if (ptr != MAP_FAILED && isAlignedAtPowerOf2(kRegionSize, ptr)) {
      // Success!
      alloc_hint += size;
      break;
    }
    if (ptr == MAP_FAILED) {
      munmap(ptr, size);
      if (alloc_hint >= kMMapRegionEnd) {
	outOfMemory();
      }
    }
  }

  DLOG("Allocated region %p-%p\n", ptr, ptr + size);

  Region *region = reinterpret_cast<Region*>(ptr);
  region->region_info_ = regionType;
  region->region_link_ = NULL;
  region->initBlocks();

  return region;
}

void Region::initBlocks() {
  // Mark all blocks as free.
  char *ptr = reinterpret_cast<char*>(this) + sizeof(Region);
  for (Word i = 0; i < kBlocksPerRegion; i++) {
    blocks_[i].flags_ = Block::kUninitialized;
    blocks_[i].start_ = ptr;
    blocks_[i].free_ = ptr;
    ptr = alignToBlockBoundary(ptr + 1);
    blocks_[i].end_ = ptr;
    blocks_[i].link_ = &blocks_[i+1];
  }
  blocks_[kBlocksPerRegion - 1].link_ = NULL; // Overwrite last link
  next_free_ = &blocks_[0];
}

static void Region::operator delete(void *p) {
  // destructor has already been called.  Nothing to be done here.
}

Region::~Region() {
  char *ptr = reinterpret_cast<char*>(this);
  DLOG("Freeing region %p-%p\n", ptr, ptr + kRegionSize);

  munmap(ptr, kRegionSize);
}

Block *Region::grabFreeBlock() {
  if (next_free_ == NULL) return NULL;

  Block *b = next_free_;
  next_free_ = b->link_;
  b->link_ = NULL;

  DLOG("Returning block %p-%p\n", b->start(), b->end());

  return b;
}

MemoryManager::MemoryManager()
  : full_(NULL), free_(NULL) {
  region_ = Region::newRegion(Region::kSmallObjectRegion);
  info_tables_ = grabFreeBlock(Block::kInfoTables);
}

MemoryManager::~MemoryManager() {
  Region *r = region_;
  while (r != NULL) {
    Region *next = r->region_link_;
    delete r;
    r = next;
  }
}

Block *MemoryManager::grabFreeBlock(Block::BlockFlags flags) {
  // 1. Try to grab a block from the free block list (very likely).
  Block *b = NULL;
  if (free_ != NULL) {
    b = free_;
    free_ = b->link_;
    b->link_ = NULL;
    b->flags_ = static_cast<uint32_t>(flags);
    return b;
  }

  // 2. Try to grab the free block from the most recently allocated
  // region.
  b = region_->grabFreeBlock();

  while (b == NULL) {
    // 3. If that failed, request more memory from the OS.
    Region *r = Region::newRegion(Region::kSmallObjectRegion);
    r->region_link_ = region_;
    region_ = r;
    b = r->grabFreeBlock();
  }

  b->flags_ = static_cast<uint32_t>(flags);
  return b;
}

void *MemoryManager::allocInto(Block **block, size_t bytes) {
  char *ptr = (*block)->alloc(bytes);
  while (LC_UNLIKELY(ptr == NULL)) {
    Block *b = grabFreeBlock((*block)->contents());
    b->link_ = *block;
    *block = b;
    ptr = b->alloc(bytes);
  }
  return ptr;
}
