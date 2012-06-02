#ifndef _MEMORYMANAGER_H_
#define _MEMORYMANAGER_H_

#include "common.hh"
#include "utils.hh"

namespace lambdachine {

// Only one OS thread should allocate to each block.

class Block
{
public:
  // Start of data in this block.
  inline char *start() { return start_; }

  // First free byte in the block.
  inline char *free() { return free_; }

  // Points past last free byte.
  inline char *end() { return end_; }
  
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

  typedef enum {
    kUninitialized = 0,
    kClosures = 1,
    kInfoTables = 2,
    kStaticClosures = 3,
    kStrings = 4,
    kBytecode = 5,
    kContentsMask = 0xff,
    kScavenged = 0x100,
    kFull = 0x200,
  } BlockFlags;

  inline BlockFlags flags() {
    return static_cast<BlockFlags>(flags_);
  }

  inline BlockFlags contents() {
    return static_cast<BlockFlags>(flags_ & kContentsMask);
  }

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
private:
  void *allocInto(Block **, size_t bytes);
  Block *grabFreeBlock(Block::BlockFlags);

  Region *region_;
  Block *full_;
  Block *free_;
  Block *info_tables_;
};

}

#endif /* _MEMORYMANAGER_H_ */
