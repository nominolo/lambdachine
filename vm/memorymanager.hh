#ifndef _MEMORYMANAGER_H_
#define _MEMORYMANAGER_H_

#include "common.hh"
#include "utils.hh"
#include "objects.hh"
#include <iostream>
#include <string.h>
// TODO: Add autoconf check for module name
#include <tr1/unordered_set>

_START_LAMBDACHINE_NAMESPACE

class MemoryManager;
class Capability;

// Only one OS thread should allocate to each block.

class Block
{
public:
  // Start of data in this block.
  inline char *start() const { return start_; }

  // First free byte in the block.
  inline char *free() const { return free_; }

  // Points past last free byte.
  inline char *end() const { return end_; }
  
  // Size of block in bytes.
  inline size_t size() const { return (size_t)(end() - start()); }

  // Allocate a number of bytes in this block.  Returns NULL
  // if no room left.
  inline char *alloc(size_t bytes) {
    char *ptr = free_;
    free_ += bytes;
    if (LC_UNLIKELY(free_ > end_)) {
      free_ = ptr;
      return NULL;
    }
    return ptr;
  }

  static const int kBlockSizeLog2 = 15; // 32K
  static const size_t kBlockSize = ((size_t)1) << kBlockSizeLog2;
  static const Word kBlockMask = kBlockSize - 1;

#define DEFINE_CONTENT_TYPE(_) \
  _(Uninitialized,  FREE) \
  _(Closures,       HEAP) \
  _(StaticClosures, STAT) \
  _(InfoTables,     INFO) \
  _(Strings,        STRG) \
  _(Bytecode,       CODE) \
  _(Metadata,       META)

  typedef enum {
#define DEF_CONTENT_CONST(name, shortname) k##name,
    DEFINE_CONTENT_TYPE(DEF_CONTENT_CONST)
    kMaxContentType,
#undef DEF_CONTENT_CONST
    kContentsMask = 0xff,
    kScavenged = 0x100,
    kFull = 0x200,
  } Flags;

  inline Flags flags() const {
    return static_cast<Flags>(flags_);
  }

  inline bool getFlag(Flags flag) const { return flags_ & (uint32_t)flag; }

  inline Flags contents() const {
    return static_cast<Flags>(flags_ & kContentsMask);
  }

  friend std::ostream& operator<<(std::ostream&, const Block&);

private:
  friend class Region;
  friend class MemoryManager;
  Block() {}; // Hidden
  ~Block() {};
  
  inline void setFlag(Flags flag) { flags_ |= (uint32_t)flag; }
  inline void clearFlag(Flags flag) { flags_ &= ~((uint32_t)flag); }
  inline void markAsFree() {
    flags_ = (uint32_t)Block::kUninitialized;
    free_ = start_;
#if !defined(NDEBUG)
    memset(free_, 0, end_ - free_);
#endif
  }
  void operator delete(void *) {}; // Forbid deleting Blocks

  char *start_;
  char *end_;
  char *free_;
  Block *link_;
  uint32_t flags_;
#if LC_ARCH_BITS == 64
  uint32_t padding;
#endif
};


class Region {
public:
  typedef enum {
    kSmallObjectRegion = 1, // The region is subdivided into blocks.
    kLargeObjectRegion	// The region contains large objects.
  } RegionType;

  static const int kRegionSizeLog2 = 20; /* 1MB */
  static const size_t kRegionSize = 1UL << kRegionSizeLog2;
  static const Word kBlocksPerRegion = kRegionSize / Block::kBlockSize;
  static const Word kRegionMask = kRegionSize - 1;

  // Allocate a new memory region from the OS.
  static Region *newRegion(RegionType);

  static inline char* alignToRegionBoundary(char *ptr) {
    Word w = reinterpret_cast<Word>(ptr);
    return
      reinterpret_cast<char*>(roundUpToPowerOf2(kRegionSizeLog2, w));
  };

  static inline char* alignToBlockBoundary(char *ptr) {
    Word w = reinterpret_cast<Word>(ptr);
    return
      reinterpret_cast<char*>
        (roundUpToPowerOf2(Block::kBlockSizeLog2, w));
  };

  static inline Region *regionFromPointer(void *p) {
    return reinterpret_cast<Region*>((Word)p & ~kRegionMask);
  }

  static inline Block *blockFromPointer(void *p) {
    Region *r = regionFromPointer(p);
    LC_ASSERT(r->isSmallObjectRegion());
    SmallObjectRegionData *rd = r->smallSelf();
    Word index = ((Word)p & kRegionMask) >> Block::kBlockSizeLog2;
    LC_ASSERT(0 <= index && index < kBlocksPerRegion);
    return &rd->blocks_[index];
  }

  // Unlink and return a free block from the region.
  //
  // Returns NULL if this region has no more free blocks.
  Block *grabFreeBlock();

  static void operator delete(void *p);
  ~Region();

  friend std::ostream& operator<<(std::ostream& out, const Region&);
  friend std::ostream& operator<<(std::ostream& out, const MemoryManager&);

  inline const char *regionId() const { return (const char*)this; }

private:
  Region() {}  // Hidden

  typedef struct {
    Word magic_;
    Word region_info_;
    Region *region_link_;
  } RegionHeader;

  typedef struct _SmallObjectRegionData {
    RegionHeader header_;
    Block blocks_[Region::kBlocksPerRegion];
    Block *next_free_;
  } SmallObjectRegionData;

  typedef struct {
    RegionHeader header_;
    char *end_;
    char *free_;
  } LargeObjectRegionData;

  inline bool isSmallObjectRegion() const {
    return meta_.region_info_ == kSmallObjectRegion;
  }

  static void initBlocks(SmallObjectRegionData *);

  inline bool inRegion(void *p) {
    return (void *)this <= p &&
      p < (void *)((char *)this + kRegionSize);
  }

  inline SmallObjectRegionData *smallSelf() const {
    LC_ASSERT(isSmallObjectRegion());
    return (SmallObjectRegionData *)&meta_;
  }

  inline LargeObjectRegionData *largeSelf() const {
    LC_ASSERT(!isSmallObjectRegion());
    return (LargeObjectRegionData *)&meta_;
  }

#define REGION_MAGIC 0x7413828213897431UL

  // Pointers Regions are cast to the proper region metadata type.
  RegionHeader meta_;

  friend class MemoryManager;
};

class AllocInfoTableHandle; // forward decl

class MemoryManager
{
  //  void *allocInfoTable(Word nwords);
public:
  MemoryManager();
  ~MemoryManager();

  InfoTable *allocInfoTable(AllocInfoTableHandle&, Word nwords);

  inline char *allocString(size_t length) {
    return reinterpret_cast<char*>(allocInto(&strings_, length + 1));
  }

  inline Closure *allocStaticClosure(size_t payloadSize) {
    return static_cast<Closure*>
      (allocInto(&static_closures_,
                 (wordsof(ClosureHeader) + payloadSize) * sizeof(Word)));
  }

  inline void *allocCode(size_t instrs, size_t bitmaps) {
    return allocInto(&bytecode_,
                     sizeof(BcIns) * instrs + sizeof(u2) * bitmaps);
  }

  inline Closure *allocClosure(InfoTable *info, size_t payloadWords) {
    Closure *cl = reinterpret_cast<Closure*>
      (allocInto(&closures_,
                 (wordsof(ClosureHeader) + payloadWords) * sizeof(Word)));
    Closure::initHeader(cl, info);
    return cl;
  }

  bool looksLikeInfoTable(void *p);
  bool looksLikeClosure(void *p);

  unsigned int infoTables();

  friend std::ostream& operator<<(std::ostream& out, const MemoryManager&);
  void debugPrint();

  inline uint64_t allocated() const { return allocated_; }
  inline uint32_t numGCs() const { return num_gcs_; };

  static const u4 kNoMask = ~0;

  inline void setTopOfStackMask(u4 mask) {
    topOfStackMask_ = mask;
  };

  static const u4 kDefaultGCTrigger = 2;  // blocks

  inline bool gcInProgress() const { return nextGC_ == 0; }

  // TODO: This API should be made better or private.
  inline void setNextGC(u4 blocks) {
    LC_ASSERT(blocks > 0);
    nextGC_ = blocks;
  }

  inline void setMinHeapSize(size_t bytes) {
    minHeapSize_ = idivCeil(bytes, Block::kBlockSize);
    if (minHeapSize_ < 2) minHeapSize_ = 2;
  }

private:
  inline void *allocInto(Block **block, size_t bytes) {
    char *ptr = (*block)->alloc(bytes);
    while (LC_UNLIKELY(ptr == NULL)) {
      blockFull(block);
      ptr = (*block)->alloc(bytes);
    }
    allocated_ += bytes;
    return ptr;
  }

  inline bool isGCd(Block *block) const {
    return block->contents() == Block::kClosures;
  }

  friend class Capability;

  // There must not be any other allocation occurring to this block.
  inline void getBumpAllocatorBounds(char **heap, char **heaplim) {
    *heap = closures_->free();
    *heaplim = closures_->end();
    LC_ASSERT(isWordAligned(*heap));
    LC_ASSERT(isWordAligned(*heaplim));
  }

  inline void sync(char *heap, char *heaplim) {
    // heaplim == NULL can happen if we want to force a thread to
    // yield.
    LC_ASSERT(heaplim == NULL || heaplim == closures_->end());
    LC_ASSERT(closures_->free() <= heap && heap <= closures_->end());
    allocated_ += static_cast<uint64_t>(heap - closures_->free());
    closures_->free_ = heap;
  }

  void bumpAllocatorFull(char **heap, char **heaplim, Capability *cap);

  // Returns non-zero if GC is necessary.  If result is 0, then *heap
  // and *heaplim point to a new block.
  int bumpAllocatorFullNoGC(char **heap, char **heaplim);

  bool markBlockReadOnly(const Block *block);
  bool markBlockReadWrite(const Block *block);

  Block *grabFreeBlock(Block::Flags);
  void blockFull(Block **);
  void performGC(Capability *cap);
  void scavengeStack(Word *base, Word *top, const BcIns *pc);
  void scavengeFrame(Word *base, Word *top, const u2 *bitmask);
  void scavengeBlock(Block *);
  void scavengeStaticRoots(Closure *);
  void scavengeLarge();
  void sweepLargeObjects();

  void evacuate(Closure **);
  void evacuateLarge(Closure *);

  // Sanity check stuff
# define SEEN_SET_TYPE std::tr1::unordered_set<void*>

  bool sanityCheckClosure(SEEN_SET_TYPE &seen, Closure *cl);
  bool sanityCheckFrame(SEEN_SET_TYPE &seen, Word *base, Word *top,
                        const u2 *bitmask);
  bool sanityCheckStack(SEEN_SET_TYPE &seen, Word *base, Word *top,
                        const BcIns *pc);
  bool sanityCheckStaticRoots(SEEN_SET_TYPE &seen, Closure *cl);
  void sanityCheckHeap(Capability *cap);
  bool inRegions(void *p);

  void beginAllocInfoTable();
  void endAllocInfoTable();

  Region *region_;
  Block *free_;
  Block *info_tables_;
  Block *static_closures_;
  Block *closures_;
  Block *strings_;
  Block *bytecode_;
  Block *old_heap_; // Only non-NULL during GC
  u4 topOfStackMask_;
  int beginAllocInfoTableLevel_;
  LargeObject *largeObjects_;
  LargeObject *evacuatedLargeObjects_;
  LargeObject *scavengedLargeObjects_;
  LargeObject *freeLargeRegions_;

  uint64_t minHeapSize_;  // in blocks
  u4 nextGC_;  // if zero, a GC gets triggered.

  // Assuming an allocation rate of 16GB/s (pretty high), this counter
  // will overflow in 2^30 seconds, or about 34 years.  That appears
  // to be fine for now (it's for statistical purposes only).
  uint64_t allocated_;
  uint64_t num_gcs_;

  friend class AllocInfoTableHandle;
};

class AllocInfoTableHandle {
public:
  AllocInfoTableHandle(MemoryManager &mm) : mm_(mm) {  mm_.beginAllocInfoTable(); }
  ~AllocInfoTableHandle() { mm_.endAllocInfoTable(); }
private:
  MemoryManager &mm_;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _MEMORYMANAGER_H_ */
