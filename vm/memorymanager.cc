#include "memorymanager.hh"
#include "utils.hh"
#include "miscclosures.hh"
#include "capability.hh"
#include "thread.hh"
#include "time.hh"

#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

_START_LAMBDACHINE_NAMESPACE

#define DLOG(...) \
  if (DEBUG_COMPONENTS & DEBUG_MEMORY_MANAGER) { \
    fprintf(stderr, "MM: " __VA_ARGS__); }

#if (DEBUG_COMPONENTS & DEBUG_MEMORY_MANAGER) != 0
#define dout cerr
#define IFDBG(stmt) stmt
#else
#define dout 0 && cerr
#define IFDBG(stmt) do {} while (0)
#endif

using namespace std;

#if !defined(MAP_ANONYMOUS) && defined(MAP_ANON)
# define MAP_ANONYMOUS          MAP_ANON
#endif

#if LC_ARCH_BITS == 64
char *const kMMapRegionStart = reinterpret_cast<char *>(0x100000000);
char *const kMMapRegionEnd = reinterpret_cast<char *>(1UL << 40); // 1 Tbyte
#elif LC_ARCH_BITS == 32
char *const kMMapRegionStart = reinterpret_cast<char *>(0x10000);
char *const kMMapRegionEnd = reinterpret_cast<char *>(1UL << 31); // 2 GB
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
  //
  // TODO: This won't work if we allow giving back memory to the OS
  // (because alloc_hint increases monotonically, so eventually we run
  // out of address space.)  Solution: Maintain a list of munmapped
  // regions and try to re-mmap them before trying to allocate at
  // alloc_hint.
  static char *alloc_hint = alignToRegionBoundary(kMMapRegionStart);
  size_t size = kRegionSize;
  char *ptr;
  uint32_t attempts = 0;

  for (;;) {
    DLOG("Trying mmap(%p-%p, %ld, ...)\n", alloc_hint, alloc_hint + size, size);

    ptr = static_cast<char *>(mmap(alloc_hint, size, kMMapProtection,
                                   kMMapFlags, -1, 0));

    if (ptr != MAP_FAILED) {
      if (isAlignedAtPowerOf2(kRegionSizeLog2, ptr)) {   // Success!
        alloc_hint += size;
        break;
      } else {
        // Check if we have enough room to make it aligned.
        char *next_aligned = alignToRegionBoundary(ptr);
        char *region_end = next_aligned + kRegionSize;
        char *alloc_end = ptr + size;

        if (region_end <= alloc_end) {
          // It fits.  Unmap the rest
          DLOG("Allocated %p-%p\n", next_aligned, region_end);
          DLOG("Trimming slop: munmap(%p-%p)\n", ptr, next_aligned);

          munmap(ptr, next_aligned - ptr);

          if (alloc_end > region_end) {
            DLOG("Trimming slop: munmap(%p-%p)\n", region_end, alloc_end);
            munmap(region_end, alloc_end - region_end);
          }

          alloc_hint = region_end;
          ptr = next_aligned;
          break;
        } else {
          munmap(ptr, size);
          alloc_hint = ptr;
          size = region_end - ptr;
        }
      }
    }

    DLOG("fail: mmap (%p) %s\n", ptr,
         ptr == MAP_FAILED ? "failed" : "not aligned");

    ++attempts;
    if (attempts > 32) {
      fprintf(stderr, "FATAL: mmap failed after %u attempts.\n", attempts);
      exit(1);
    }

    if (alloc_hint >= kMMapRegionEnd) {
      outOfMemory();
    }
  }

  DLOG("Allocated region %p-%p\n", ptr, ptr + size);

  Region *region = reinterpret_cast<Region *>(ptr);
  region->magic_ = REGION_MAGIC;
  region->region_info_ = regionType;
  region->region_link_ = NULL;
  region->initBlocks();

  return region;
}

void Region::initBlocks() {
  // Mark all blocks as free.
  char *ptr = reinterpret_cast<char *>(this) + sizeof(Region);

  // If the block size is very small, the first few blocks may not be available since
  // that is where the meta data is stored.
  Word first_avail = sizeof(Region) / Block::kBlockSize;
  
  char *metadata = reinterpret_cast<char *>(this);
  for (Word i = 0; i < first_avail; i++) {
    blocks_[i].flags_ = Block::kMetadata;
    blocks_[i].start_ = metadata;
    metadata = alignToBlockBoundary(metadata + 1);
    blocks_[i].end_ = metadata;
    blocks_[i].free_ = metadata;
    blocks_[i].link_ = NULL;
  }
  for (Word i = first_avail; i < kBlocksPerRegion; i++) {
    blocks_[i].flags_ = Block::kUninitialized;
    blocks_[i].start_ = ptr;
    blocks_[i].free_ = ptr;
    ptr = alignToBlockBoundary(ptr + 1);
    blocks_[i].end_ = ptr;
    blocks_[i].link_ = &blocks_[i + 1];
  }
  blocks_[kBlocksPerRegion - 1].link_ = NULL; // Overwrite last link
  next_free_ = &blocks_[first_avail];
}

void Region::operator delete(void *) {
  // destructor has already been called.  Nothing to be done here.
}

Region::~Region() {
  char *ptr = reinterpret_cast<char *>(this);
  DLOG("Freeing region %p-%p\n", ptr, ptr + kRegionSize);

  munmap(ptr, kRegionSize);
}

Block *Region::grabFreeBlock() {
  if (next_free_ == NULL) return NULL;

  Block *b = next_free_;
  next_free_ = b->link_;
  b->link_ = NULL;

  // DLOG("Returning block %p-%p\n", b->start(), b->end());

  return b;
}

Time gc_time = 0;

MemoryManager::MemoryManager()
  : free_(NULL), old_heap_(NULL), topOfStackMask_(kNoMask),
    beginAllocInfoTableLevel_(0),
    minHeapSize_(2), 
    nextGC_(minHeapSize_),
    allocated_(0), num_gcs_(0)
{
  region_ = Region::newRegion(Region::kSmallObjectRegion);
  static_closures_ = grabFreeBlock(Block::kStaticClosures);
  info_tables_ = grabFreeBlock(Block::kInfoTables);
  bool ok = markBlockReadOnly(info_tables_);
  LC_ASSERT(ok);
  closures_ = grabFreeBlock(Block::kClosures);
  strings_ = grabFreeBlock(Block::kStrings);
  bytecode_ = grabFreeBlock(Block::kBytecode);
}

MemoryManager::~MemoryManager() {
  Region *r = region_;
  while (r != NULL) {
    Region *next = r->region_link_;
    delete r;
    r = next;
  }
  MiscClosures::reset();
}

Block *MemoryManager::grabFreeBlock(Block::Flags flags) {
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

void MemoryManager::blockFull(Block **block) {
  Block *fullBlock = *block;
  Block *emptyBlock = grabFreeBlock(fullBlock->contents());
  dout << "BLOCK_FULL" << endl;
  emptyBlock->link_ = *block;
  *block = emptyBlock;
}

bool
MemoryManager::markBlockReadOnly(const Block *block)
{
  char *from = block->start();
  size_t size = block->size();
  DLOG( "Marking block as read-only [%p-%p]\n", from, from + size);
  // May fail in particular if "from" is not page-aligned.
  return mprotect(from, size, PROT_READ) == 0;
}

bool
MemoryManager::markBlockReadWrite(const Block *block)
{
  char *from = block->start();
  size_t size = block->size();
  DLOG( "Marking block as read-write [%p-%p]\n", from, from + size);
  return mprotect(from, size, PROT_READ | PROT_WRITE) == 0;
}

inline bool isPageAligned(const void *ptr) {
  return ((Word)ptr & 0xfff) == 0;
}

InfoTable *
MemoryManager::allocInfoTable(AllocInfoTableHandle&, Word nwords)
{
  // The fact that we got passed a AllocInfoTableHandle means that the
  // block at info_tables_ is writeable.
  LC_ASSERT(beginAllocInfoTableLevel_ > 0);

  size_t bytes = nwords * sizeof(Word);
  char *ptr = info_tables_->alloc(bytes);
  while (LC_UNLIKELY(ptr == NULL)) {
    bool ok = markBlockReadOnly(info_tables_);
    LC_ASSERT(ok && "Failed to mark block R/O");
    blockFull(&info_tables_);
    if (!isPageAligned(info_tables_->start())) {
      cerr << "TODO: Need API to request page-aligned memory.\n";
      // Just grab another block.
      continue;
    }
    ptr = info_tables_->alloc(bytes);
  }
  allocated_ += bytes;
  return (InfoTable *)ptr;
}

void
MemoryManager::beginAllocInfoTable()
{
  if (beginAllocInfoTableLevel_ == 0) {
    bool ok = markBlockReadWrite(info_tables_);
    LC_ASSERT(ok && "Failed to mark block as R/W");
  }
  ++beginAllocInfoTableLevel_;
}

void
MemoryManager::endAllocInfoTable()
{
  --beginAllocInfoTableLevel_;
  if (beginAllocInfoTableLevel_ == 0) {
    bool ok = markBlockReadOnly(info_tables_);
    LC_ASSERT(ok && "Failed to mark block R/O");
  }
}

// Returns non-zero if GC is necessary.
int
MemoryManager::bumpAllocatorFullNoGC(char **heap, char **heaplim)
{
  sync(*heap, *heaplim);
  --nextGC_;
  if (LC_UNLIKELY(nextGC_ == 0)) {
    ++nextGC_;
    return 1;
  }
  
  blockFull(&closures_);
  getBumpAllocatorBounds(heap, heaplim);
  return 0;
}

void MemoryManager::bumpAllocatorFull(char **heap, char **heaplim,
                                      Capability *cap) {
  sync(*heap, *heaplim);
  --nextGC_;
  if (LC_UNLIKELY(nextGC_ == 0)) {
    performGC(cap);
  } else {
    blockFull(&closures_);
  }
  getBumpAllocatorBounds(heap, heaplim);
  dout << "MM: heap=" << (void *)*heap
       << " heaplim=" << (void *)*heaplim
       << " nextGC=" << nextGC_
       << endl;
}

unsigned int MemoryManager::infoTables() {
  Block *b = info_tables_;
  unsigned int n = 0;
  do {
    n++;
    b = b->link_;
  } while (b != NULL);
  return n;
}

bool MemoryManager::looksLikeInfoTable(void *p) {
  Block *block = Region::blockFromPointer(p);
  return block->contents() == Block::kInfoTables;
}

void MemoryManager::debugPrint() {
  cerr << *this << endl;
}

bool MemoryManager::looksLikeClosure(void *p) {
  Block *block = Region::blockFromPointer(p);
  if (!(block->contents() == Block::kStaticClosures ||
        block->contents() == Block::kClosures))
    return false;
  Closure *cl = (Closure *)p;
  return cl->info() != NULL && looksLikeInfoTable(cl->info());
}

static char blockContentShortname[][Block::kMaxContentType] = {
#define DEF_CONTENT_STR(name, shortname) #shortname,
  DEFINE_CONTENT_TYPE(DEF_CONTENT_STR)
#undef DEF_CONTENT_STR
};

// Useful mainly for debugging.

std::ostream &operator<<(std::ostream &out, const Block &b) {
  out << blockContentShortname[b.contents()]
      << " [" << (void *)b.start() << '-' << (void *)b.end();
  size_t blockSize = static_cast<size_t>(b.end() - b.start());
  size_t blockFull = static_cast<size_t>(b.free() - b.start());
  cout << " full:" << (100 * blockFull + (blockSize / 2)) / blockSize
       << "% link:" << (void *)(b.link_ != NULL ? b.link_->start() : NULL) << "]";
  return out;
}

std::ostream &operator<<(std::ostream &out, const Region &r) {
  const char *ptr = r.regionId();
  out << "Region [" << (void *)ptr << "-"
      << (void *)(ptr + Region::kRegionSize) << "]" << endl;
  for (Word i = 0; i < Region::kBlocksPerRegion; i++) {
    out << "  " << r.blocks_[i] << endl;
  }
  return out;
}

std::ostream &operator<<(std::ostream &out, const MemoryManager &mm) {
  Region *r = mm.region_;
  while (r != NULL) {
    out << *r;
    r = r->region_link_;
  }
  return out;
}

//= Garbage Collection Stuff =========================================

void MemoryManager::performGC(Capability *cap) {
  Time gc_start = getProcessElapsedTime();
  Thread *T = cap->currentThread();
  BcIns *pc = T->pc();
  Word *base = T->base();
  Word *top = T->top();

  if (DEBUG_COMPONENTS & DEBUG_SANITY_CHECK_GC) {
    cerr << ">>> GC " << num_gcs_ << endl;
    // This ensures that the mutator hasn't introduced any corrupt
    // state.
    sanityCheckHeap(cap);
  }

  ++num_gcs_;

  LC_ASSERT(old_heap_ == NULL);
  old_heap_ = closures_;

  closures_ = grabFreeBlock(closures_->contents());
  closures_->link_ = NULL;

  // Traverse the roots.
  // TODO: Traverse updated CAFs
  scavengeStack(base, top, pc);
  scavengeStaticRoots(cap->staticRoots());

  // Scavenging allocates into closures_ and pushes filled blocks onto
  // the front of the list.  So we repeatedly traverse closures_ until
  // we reach the end of a list or a block that we've already
  // processed.
  for (;;) {
    Block *block = closures_;
    if (block == NULL || block->getFlag(Block::kScavenged)) {
      break;  // we're done
    } else {
      // If the block we are evacuating into and the block we're
      // scavanging is the same then it definitely must be the last
      // block.  Otherwise, we may finish scavenging the block
      // and then continue to evacuate into it again.  Those newly
      // evacuated objects will then never get scavenged.  Ouch!
      //
      // To avoid this problem, we only scavenge the first block if
      // there is no second block.
      if (block == closures_ && block->link_ &&
          !block->link_->getFlag(Block::kScavenged))
        block = block->link_;

      while (block != NULL && !block->getFlag(Block::kScavenged)) {
        scavengeBlock(block);
        block = block->link_;
      }
    }
  }

  // Mark blocks as no longer scavenged.
  // TODO: Would it help much to not do this?
  u4 fullBlocks = 0;
  for (Block *block = closures_; block != NULL; block = block->link_) {
    block->clearFlag(Block::kScavenged);
    ++fullBlocks;
  }

  // O
  for (Block *block = old_heap_; block != NULL; ) {
    block->markAsFree();
    Block *next = block->link_;
    block->link_ = free_;
    free_ = block;
    block = next;
  }
  old_heap_ = NULL;

  // TODO: Add sanity check.  Everything reachable from the roots must
  // be in a k[Static]Closures block now.

  // TODO: Is this correct?
  nextGC_ = (fullBlocks > minHeapSize_ ? fullBlocks : minHeapSize_) + 1;

  if (DEBUG_COMPONENTS & DEBUG_SANITY_CHECK_GC) {
    cerr << ">>> GC " << num_gcs_ - 1 << " DONE (full blocks = "
         << fullBlocks << ")\n";
    // This ensures that the collector itself hasn't introduced any
    // corrupt state.
    sanityCheckHeap(cap);
  }

  gc_time += getProcessElapsedTime() - gc_start;
}

static inline bool isForwardingPointer(const InfoTable *p) {
  return (Word)p & 1;
}

static inline Closure *getForwardingPointer(const InfoTable *p) {
  return cast(Closure *, (Word)p ^ 1);
}

static inline InfoTable *makeForwardingPointer(Closure *p) {
  return cast (InfoTable *, (Word)p | 1);
}

static inline void copy(MemoryManager *mm, Closure **src,
                        InfoTable *info, u4 payloadSize) {
  Closure *from = *src;
  Closure *to = mm->allocClosure(info, payloadSize);
  *src = to;
  for (u4 i = 0; i < payloadSize; ++i) {
    to->setPayload(i, from->payload(i));
  }
  dout << "(fwd@" << from << ")";
  from->setInfo(makeForwardingPointer(to));
  dout << COL_GREEN << to << COL_RESET << endl;
}

void MemoryManager::evacuate(Closure **p) {
  Closure *q;
  InfoTable *info;
  Block *block;

  q = *p;

  LC_ASSERT(q != NULL);
  dout << "MM: Evac: " COL_RED << q << COL_RESET;

loop:
  info = q->info();

  if (isForwardingPointer(info)) {
    *p = getForwardingPointer(info);
    dout << " -F-> " COL_YELLOW << *p << COL_RESET << endl;
    return;
  }

  dout << ' ' << info->name();

  block = Region::blockFromPointer(q);
  if (block->contents() != Block::kClosures) {
    // TODO: Need to follow indirections from static closures into
    // dynamic heap.
    dout << " -S-> " COL_YELLOW "static object" COL_RESET << endl;
    return;
  }

  switch (info->type()) {
  case CONSTR:
  case THUNK:
  case FUN:
    dout << " -CTF(" << info->size() << ")-> ";
    copy(this, p, info, info->size());
    break;

  case IND:
    q = (Closure *)q->payload(0);
    dout << " -I-> " << q;
    *p = q;
    goto loop;

  case PAP: {
    PapClosure *pap = (PapClosure *)q;
    u4 size = pap->info_.nargs_ + wordsof(PapClosure)
              - wordsof(ClosureHeader);
    dout << " -PAP(" << pap->info_.nargs_ << ")-> " << pap;
    copy(this, p, info, size);
  }
  break;

  default:
    dout << " -cannot evacuate yet: " << info->type() << endl;
    exit(44);
  }
}

void MemoryManager::scavengeFrame(Word *base, Word *top, const u2 *bitmaps) {
  dout << "Scavenging frame " << base << '-' << top << endl;
  dout << "-1:";
  evacuate((Closure **)&base[-1]); // The frame node
  if (bitmaps == NULL)
    return;
  u2 bitmap;
  int slot = 0;
  ptrdiff_t slots = top - base;
  do {
    bitmap = *bitmaps;
    ++bitmaps;
    for (int i = 0; i < 15 && bitmap != 0; ++i, bitmap >>= 1, ++slot) {
      if (bitmap & 1) {
        LC_ASSERT(slot < slots);
        dout << slot << ": ";
        evacuate((Closure **)&base[slot]);
      }
    }
  } while (bitmap != 0);
}

static inline const u2 *topFrameBitmask(const BcIns *pc) {
  const BcIns ins = *pc;
  switch (ins.opcode()) {
  case BcIns::kALLOC1:
    return BcIns::offsetToBitmask(pc + 1);
  case BcIns::kALLOC:
    return BcIns::offsetToBitmask(pc + 1 + BC_ROUND(ins.c()));
  case BcIns::kALLOCAP:
    return BcIns::offsetToBitmask(pc + 1 + BC_ROUND((u4)ins.c() + 1));
  default:
    cerr << "FATAL: Instruction should not have triggered GC: " << ins.name() << endl;
    exit(1);
  }
}

void MemoryManager::scavengeStack(Word *base, Word *top, const BcIns *pc) {
  u2 dummy_mask[3];
  const u2 *bitmask;
  if (topOfStackMask_ != kNoMask) {
    // Translate the topOfStackMask into our standard format.  We're
    // starting a full GC anyway, so it doesn't matter if this is a
    // little bit inefficient.
    dummy_mask[0] = topOfStackMask_ & 0x7fff;
    dummy_mask[1] = (topOfStackMask_ >> 15) & 0x7fff;
    dummy_mask[2] = (topOfStackMask_ >> 30);
    if (dummy_mask[2] != 0) dummy_mask[1] |= 0x8000;
    if (dummy_mask[1] != 0) dummy_mask[0] |= 0x8000;
    bitmask = &dummy_mask[0];
  } else {
    bitmask = topFrameBitmask(pc);
  }
  scavengeFrame(base, top, bitmask);
  top = base - 3;
  pc = (BcIns *)base[-2];
  base = (Word *)base[-3];

  while (base) {
    scavengeFrame(base, top, BcIns::offsetToBitmask(pc - 1));
    top = base - 3;
    pc = (BcIns *)base[-2];
    base = (Word *)base[-3];
  }
}

void MemoryManager::scavengeStaticRoots(Closure *cl) {
  dout << "MM: Scavenging static roots" << endl;
  while (cl) {
    evacuate((Closure **)&cl->payload_[0]);
    cl = (Closure *)cl->payload_[1];
  }
}

void MemoryManager::scavengeBlock(Block *block) {
  dout << "MM: Scavenging block: " << (void *)block->start()
       << '-' << (void *)block->end() << endl;

  char *p = block->start();

  // We might be evacuating into the same block that we're scavenging.
  // That is `bd->free` might change during the loop, so recheck here.
  while ((char *)p < block->free()) {
    Closure *cl = (Closure *)p;
    InfoTable *info = cl->info();
    LC_ASSERT(!isForwardingPointer(info));
    switch (info->type()) {
    case CONSTR:
    case THUNK:
    case FUN: {
      u4 bitmap = info->layout().bitmap;
      u4 size = info->size();
      dout << "MM: * Scav " << (void *)cl
           << ' ' << info->name() << ' ';
      IFDBG(InfoTable::printPayload(dout, bitmap, size));
      dout << endl;

      LC_ASSERT(bitmap < (1UL << size));
      for (u4 i = 0; bitmap != 0 && i < size; ++i, bitmap >>= 1) {
        if (bitmap & 1) {
          evacuate((Closure **)&cl->payload_[i]);
        }
      }
      p += (wordsof(ClosureHeader) + size) * sizeof(Word);
    }
    break;

    case PAP: {
      PapClosure *pap = (PapClosure *)cl;
      // In principle we could get the bitmap from the function
      // argument itself.  That would require following a few more
      // pointers, though, so let's not do that if we can avoid it.
      u4 bitmap = pap->info_.pointerMask_;
      u4 size = pap->info_.nargs_;
      dout << "MM: * Scav " << (void *)cl << " PAP";
      IFDBG(InfoTable::printPayload(dout, bitmap, size));
      dout << endl;

      evacuate(&pap->fun_);

      LC_ASSERT(bitmap < (1UL << size));
      for (u4 i = 0; bitmap != 0 && i < size; ++i, bitmap >>= 1) {
        if (bitmap & 1) {
          evacuate((Closure **)&pap->payload_[i]);
        }
      }
      p += (wordsof(PapClosure) + size) * sizeof(Word);
    }
    break;

    default:
      cerr << "Can't scavenge object type, yet: " << info->type()
           << " at " << cl << " " << info->name()
           << endl;
      exit(43);
    }
  }
  block->setFlag(Block::kScavenged);
  dout << "MM: DONE Scavenging block: " << (void *)block->start()
       << '-' << (void *)block->end() << endl;
}

// --- Sanity Checking ----------------------------------------
//
// This checks the heap for basic properties.  It should help us
// detect any problems in the garbage collector or allocator.  Sanity
// checking traverses the whole heap, so it is very slow.

bool MemoryManager::sanityCheckClosure(SEEN_SET_TYPE &seen, Closure *cl) {
  void *p = (void *)cl;
  if (seen.count(p) > 0)
    return true;

  if (!looksLikeClosure(p)) {
    cerr << "Not a closure: " << p << endl;
    return false;
  }

  seen.insert((void *)cl);
  InfoTable *info = cl->info();

  if (isForwardingPointer(info)) {
    cerr << "Info table of " << p << " is a forwarding pointer: "
         << (void *)info << endl;
    return false;
  }

  switch (info->type()) {
    case CONSTR:
    case THUNK:
    case CAF:
    case FUN: {
      u4 bitmap = info->layout().bitmap;
      u4 size = info->size();
      
      if (!(bitmap < (1UL << size))) {
        cerr << "Bitmap " << hex << bitmap << " inconsistent with size "
             << dec << size << " in closure " << p << endl;
        return false;
      }
      for (u4 i = 0; bitmap != 0 && i < size; ++i, bitmap >>= 1) {
        if (bitmap & 1) {
          if (!sanityCheckClosure(seen, (Closure *)cl->payload_[i])) {
            cerr << ".. " << p << '[' << i << "] " << info->name() << endl;
            return false;
          }
        }
      }
      break;
    }

  case IND: {
    if (!sanityCheckClosure(seen, (Closure *)cl->payload(0))) {
      cerr << ".. IND " << p << endl;
      return false;
    }
    break;
  }

  case UPDATE_FRAME:
  case AP_CONT:
    break;

    case PAP: {
      PapClosure *pap = (PapClosure *)cl;
      // In principle we could get the bitmap from the function
      // argument itself.  That would require following a few more
      // pointers, though, so let's not do that if we can avoid it.
      u4 bitmap = pap->info_.pointerMask_;
      u4 size = pap->info_.nargs_;

      if (!sanityCheckClosure(seen, pap->fun_)) {
        cerr << ".. " << p << " pap function\n";
        return false;
      }

      if (!(bitmap < (1UL << size))) {
        cerr << "Bitmap " << hex << bitmap << " inconsistent with size "
             << dec << size << " in PAP closure " << p << endl;
        return false;
      }

      for (u4 i = 0; bitmap != 0 && i < size; ++i, bitmap >>= 1) {
        if (bitmap & 1) {
          if (!sanityCheckClosure(seen, (Closure *)pap->payload_[i])) {
            cerr << ".. " << p << '[' << i << "] PAP payload" << endl;
            return false;
          }
        }
      }
    }
    break;

  default:
    cerr << "Unknown closure type: " << (int)info->type() << " at "
         << p << endl;
    return false;
  }
  return true;
}

bool MemoryManager::sanityCheckFrame(SEEN_SET_TYPE &seen, Word *base, Word *top,
                                     const u2 *bitmaps) {
  if (!sanityCheckClosure(seen, (Closure *)base[-1])) {
    cerr << ".. frame node of frame " << base << '-' << top << endl;
    return false;
  }

  if (bitmaps == NULL) {
    cerr << "Frame bitmaps are NULL, in frame " << base << '-' << top << endl;
    return false;
  }

  u2 bitmap;
  int slot = 0;
  ptrdiff_t slots = top - base;
  do {
    bitmap = *bitmaps;
    ++bitmaps;
    for (int i = 0; i < 15 && bitmap != 0; ++i, bitmap >>= 1, ++slot) {
      if (bitmap & 1) {
        if (!(slot < slots)) {
          cerr << "Frame bitmaps larger than frame size (" << slots
               << " in frame " << base << '-' << top << endl;
          return false;
        }

        if (!(sanityCheckClosure(seen, (Closure *)base[slot]))) {
          cerr << ".. slot " << slot << " of frame "
               << base << '-' << top << endl;
          return false;
        }
      }
    }
  } while (bitmap != 0);

  return true;
}

bool MemoryManager::sanityCheckStack(SEEN_SET_TYPE &seen, Word *base, Word *top,
                                     const BcIns *pc)
{
    u2 dummy_mask[3];
  const u2 *bitmask;
  if (topOfStackMask_ != kNoMask) {
    // Translate the topOfStackMask into our standard format.  We're
    // starting a full GC anyway, so it doesn't matter if this is a
    // little bit inefficient.
    dummy_mask[0] = topOfStackMask_ & 0x7fff;
    dummy_mask[1] = (topOfStackMask_ >> 15) & 0x7fff;
    dummy_mask[2] = (topOfStackMask_ >> 30);
    if (dummy_mask[2] != 0) dummy_mask[1] |= 0x8000;
    if (dummy_mask[1] != 0) dummy_mask[0] |= 0x8000;
    bitmask = &dummy_mask[0];
  } else {
    bitmask = topFrameBitmask(pc);
  }
  if (!sanityCheckFrame(seen, base, top, bitmask))
    return false;
  
  top = base - 3;
  pc = (BcIns *)base[-2];
  base = (Word *)base[-3];

  while (base) {
    if (!sanityCheckFrame(seen, base, top, BcIns::offsetToBitmask(pc - 1)))
      return false;
    top = base - 3;
    pc = (BcIns *)base[-2];
    base = (Word *)base[-3];
  }
  
  return true;
}

bool MemoryManager::sanityCheckStaticRoots(SEEN_SET_TYPE &seen, Closure *cl) {
  while (cl) {
    if (!sanityCheckClosure(seen, (Closure *)cl->payload_[0])) {
      cerr << ".. static root " << (void *)cl;
      return false;
    }
    cl = (Closure *)cl->payload_[1];
  }
  return true;
}

void MemoryManager::sanityCheckHeap(Capability *cap) {
  SEEN_SET_TYPE seen;

  Thread *T = cap->currentThread();
  BcIns *pc = T->pc();
  Word *base = T->base();
  Word *top = T->top();

  if (!(sanityCheckStack(seen, base, top, pc) &&
        sanityCheckStaticRoots(seen, cap->staticRoots())))
    exit(42);
}

bool
MemoryManager::inRegions(void *p)
{
  Region *r = region_;
  while (r) {
    if (r->inRegion(p))
      return true;
    r = r->region_link_;
  }
  return false;
}

_END_LAMBDACHINE_NAMESPACE
