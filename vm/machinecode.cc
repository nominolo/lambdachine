#include "jit.hh"

#include <sys/mman.h>

/* Define this if you want to run Lambdachine with Valgrind. */
#ifdef LC_USE_VALGRIND
#include <valgrind/valgrind.h>
#endif


_START_LAMBDACHINE_NAMESPACE

using namespace std;

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS MAP_ANON
#endif

#define MCPROT_RW (PROT_READ|PROT_WRITE)
#define MCPROT_RX (PROT_READ|PROT_EXEC)
#define MCPROT_RWX  (PROT_READ|PROT_WRITE|PROT_EXEC)

#define MCPROT_GEN  MCPROT_RW
#define MCPROT_RUN  MCPROT_RX

#define LC_TARGET_JUMPRANGE 31


MachineCode::MachineCode(Prng *prng)
  : prng_(prng),
    protection_(0),
    area_(NULL), top_(NULL), bottom_(NULL),
    size_(0), sizeTotal_(0) {
}

MachineCode::~MachineCode() {
  this->free(area_, size_);
}

static inline bool isValidMachineCodePtr(void *p) {
  return p && ((uintptr_t)p < (uintptr_t)1 << 47);
}

void *MachineCode::alloc(size_t size) {
  // TODO: If there we have an existing machine code area, allocate
  // a new one within jump-range of any existing areas.  For now, we just
  // allocate a large area and then fail if we need more.

  // The exit handler must be reachable from the generated code.
  uintptr_t target = (uintptr_t)(void *)&asmExit & ~(uintptr_t)0xffff;
  const uintptr_t range = (1u << LC_TARGET_JUMPRANGE) - (1u << 21);
  uintptr_t hint = 0;
  for (int i = 0; i < 32; ++i) {
    if (isValidMachineCodePtr((void *)hint)) {
      void *p = allocAt(hint, size, MCPROT_GEN);
      if (isValidMachineCodePtr(p)) {
        // See if it's in range.
        if ((uintptr_t)p + size - target < range ||
            target - (uintptr_t)p < range)
          return p;
        this->free(p, size);
      }
    }

    // Try a random other address.
    do {
      hint = (0x78fb ^ prng_->bits(15)) << 16;  /* 64K aligned. */
    } while (!(hint + size < range));
    hint = target + hint - (range >> 1);
  }
  cerr << "FATAL: Could not allocate memory for machine code." << endl;
  return NULL;
}

void *MachineCode::allocAt(uintptr_t hint, size_t size, int prot) {
  void *p = mmap((void *)hint, size, prot, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED && !hint) {
    cerr << "Failed to allocate memory from OS." << endl;
    exit(23);
  }
  return p;
}

void MachineCode::free(void *p, size_t size) {
  munmap(p, size);
}

void MachineCode::setProtection(void *p, size_t size, int prot) {
  mprotect(p, size, prot);
}

MCode *MachineCode::reserve(MCode **limit) {
  if (area_ == NULL)
    allocArea();
  else
    protect(MCPROT_GEN);
  *limit = bottom_;
  return top_;
}

void MachineCode::allocArea() {
  // TODO: We only create one large machine code area.  In the future
  // it would be nice to have a linked list of areas.
  size_t size = kAreaSize;
  // Round up to page size.
  size = (size + LC_PAGESIZE - 1) & ~(size_t)(LC_PAGESIZE - 1);
  area_ = (MCode *)alloc(size);
  size_ = size;
  sizeTotal_ = size;
  protection_ = MCPROT_GEN;
  bottom_ = area_;
  top_ = (MCode *)((char *)area_ + size_);
}

void MachineCode::commit(MCode *top) {
  LC_ASSERT(top <= (char *)area_ + size_);
  LC_ASSERT(bottom_ <= top);
  top_ = top;
  protect(MCPROT_RUN);
}

void MachineCode::commitStub(MCode *bot) {
  LC_ASSERT(bot >= bottom_);
  LC_ASSERT(bot <= top_);
  bottom_ = bot;
}

void MachineCode::abort() {
  protect(MCPROT_RUN);
}

void MachineCode::protect(int prot) {
  if (protection_ != prot) {
    setProtection(area_, size_, prot);
    protection_ = prot;
  }
}

void MachineCode::syncCache(void *start, void *end) {
#ifdef LUAJIT_USE_VALGRIND
  VALGRIND_DISCARD_TRANSLATIONS(start, (char *)end - (char *)start);
#endif

#if LC_TARGET_X86ORX64
  // x86 ensures cache consistency automatically.
  UNUSED(start);
  UNUSED(end);

// #elif LJ_TARGET_IOS
//   sys_icache_invalidate(start, (char *)end-(char *)start);
// #elif LJ_TARGET_PPC
//   lj_vm_cachesync(start, end);

#elif defined(__GNUC__)
  // XXX: Seems to be broken on Android/ARM.  Workaround at:
  // http://code.google.com/p/android/issues/detail?id=1803
  __clear_cache(start, end);
#else
#error "Missing/Unimplemented builtin to flush instruction cache"
#endif
}


void MachineCode::dumpAsm(ostream &out) {
  out << ".text\ndump:\n";
  dumpAsm(out, start(), end());
}

void MachineCode::dumpAsm(ostream &out, MCode *from, MCode *to) {
  MCode *p = from;
  for ( ; p < to; ++p) {
    out << "\t.byte 0x" << hex << (int)(uint8_t)*p << dec << endl;
  }
}

_END_LAMBDACHINE_NAMESPACE
