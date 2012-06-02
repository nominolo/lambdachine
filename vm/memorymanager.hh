#ifndef _MEMORYMANAGER_H_
#define _MEMORYMANAGER_H_

#include "common.hh"
#include "utils.hh"
#include "objects.hh"
#include <iostream>

namespace lambdachine {

class MemoryManager;

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
  _(InfoTables,     INFO) \
  _(StaticClosures, STAT) \
  _(Strings,        STRG) \
  _(Bytecode,       CODE)

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

  inline Flags contents() const {
    return static_cast<Flags>(flags_ & kContentsMask);
  }

  friend std::ostream& operator<<(std::ostream&, const Block&);

private:
  friend class Region;
  friend class MemoryManager;
  Block() {}; // Hidden
  ~Block() {};
  void operator delete(void *p) {}; // Forbid deleting Blocks

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
    //    kLargeObjectRegion,	// The region contains large objects.
  } RegionType;

  static const int kRegionSizeLog2 = 20; /* 1MB */
  static const size_t kRegionSize = 1UL << kRegionSizeLog2;
  static const Word kBlocksPerRegion = kRegionSize / Block::kBlockSize;

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
  void initBlocks();

  Word region_info_;
  Block blocks_[kBlocksPerRegion];
  Region *region_link_;
  Block *next_free_;

  friend class MemoryManager;
};

class MemoryManager
{
  //  void *allocInfoTable(Word nwords);
public:
  MemoryManager();
  ~MemoryManager();

  inline void *allocInfoTable(Word nwords) {
    return allocInto(&info_tables_, nwords * sizeof(Word));
  }

  inline void *allocStaticClosure(Word nwords) {
    return allocInto(&static_closures_, nwords * sizeof(Word));
  }

  inline void *allocClosure(InfoTable *info, Word nPayloadWords) {
    Closure *cl = reinterpret_cast<Closure*>
      (allocInto(&closures_, wordsof(ClosureHeader) + nPayloadWords * sizeof(Word)));
    Closure::initHeader(cl, info);
    return cl;
  }

  unsigned int infoTables();

  friend std::ostream& operator<<(std::ostream& out, const MemoryManager&);

private:
  inline void *allocInto(Block **block, size_t bytes) {
    char *ptr = (*block)->alloc(bytes);
    while (LC_UNLIKELY(ptr == NULL)) {
      ptr = blockFull(block, bytes);
    }
    return ptr;
  }

  inline bool isGCd(Block *block) const {
    return block->contents() == Block::kClosures;
  }

  Block *grabFreeBlock(Block::Flags);
  char *blockFull(Block **, size_t bytes);

  Region *region_;
  Block *full_;
  Block *free_;
  Block *info_tables_;
  Block *static_closures_;
  Block *closures_;
};

}

#endif /* _MEMORYMANAGER_H_ */
