#include "jit.hh"

#include <iostream>
#include <sys/mman.h>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

HotCounters::HotCounters(HotCount threshold)
  : threshold_(threshold) {
  for (int i = 0; i < kNumCounters; ++i) {
    counters_[i] = threshold;
  }
}

Jit::Jit()
  : cap_(NULL), startPc_(NULL), startBase_(NULL),
    flags_(0), targets_(), fragments_(),
    prng_(0x72ba83e) // TODO: Hard-coded for easier debugging.
{
}

Jit::~Jit() {
  for (FRAGMENT_MAP::iterator it = fragments_.begin();
       it != fragments_.end(); ++it) {
    delete it->second;
  }
}

void Jit::beginRecording(Capability *cap, BcIns *startPc, Word *base, bool isReturn) {
  LC_ASSERT(cap_ == NULL);
  LC_ASSERT(targets_.size() == 0);
  cap_ = cap;
  startPc_ = startPc;
  startBase_ = base;
  flags_.clear(kLastInsWasBranch);
  flags_.set(kIsReturnTrace, isReturn);
}

bool Jit::recordIns(BcIns *ins, Word *base) {
  cerr << "REC: " << ins << " " << ins->name() << endl;
  if (flags_.get(kLastInsWasBranch)) {
    if (ins == startPc_) {
      cerr << "REC: Loop detected." << endl
           << "  Loop: " << startPc_ << endl;
      for (int i = 0; i < targets_.size(); ++i) {
        cerr << "    " << targets_[i] << endl;
      }
      finishRecording();
      return true;
    } else if (targets_.size() <= 100) {
      // Try to find inner loop.
      for (int i = 0; i < targets_.size(); ++i) {
        if (targets_[i] == ins) {
          cerr << COL_GREEN << "REC: Inner loop." << COL_RESET << endl;

          // TODO: Truncate.

          resetRecorderState();
          return true;
        }
      }

      targets_.push_back(ins);
    } else {
      cerr << COL_RED << "TRACE TOO LONG (" << targets_.size()
           << ")" << COL_RESET << endl;
      cerr << "    " << startPc_ << endl;
      for (int i = 0; i < targets_.size(); ++i) {
        if ((i % 7) == 0) { cerr << "   "; }
        cerr << ' ' << i << ':' << targets_[i];
        if (((i + 1) % 7) == 0) cerr << endl;
      }
      cerr << endl;

      resetRecorderState();
      return true;
    }
  }

  switch (ins->opcode()) {
  case BcIns::kCALL:
  case BcIns::kCALLT:
  case BcIns::kEVAL:
  case BcIns::kRET1:
    flags_.set(kLastInsWasBranch);
    break;
  default:
    flags_.clear(kLastInsWasBranch);
    break;
  }
  return false;
}

inline void Jit::resetRecorderState() {
  flags_.clear();
  targets_.clear();
  cap_ = NULL;
}

void Jit::finishRecording() {
  Fragment *F = new Fragment();
  F->traceId_ = fragments_.size();
  F->numTargets_ = targets_.size();
  F->targets_ = new BcIns*[F->numTargets_];
  F->startPc_ = startPc_;
  for (int i = 0; i < F->numTargets_; ++i) {
    F->targets_[i] = targets_[i];
  }
  registerFragment(startPc_, F);
  resetRecorderState();
}

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS	MAP_ANON
#endif

#define MCPROT_RW	(PROT_READ|PROT_WRITE)
#define MCPROT_RX	(PROT_READ|PROT_EXEC)
#define MCPROT_RWX	(PROT_READ|PROT_WRITE|PROT_EXEC)

#define MCPROT_GEN	MCPROT_RW
#define MCPROT_RUN	MCPROT_RX

#define LC_TARGET_JUMPRANGE 31

static inline bool isValidMachineCodePtr(void *p) {
  return p && ((uintptr_t)p < (uintptr_t)1 << 47);
}

void *Jit::allocMachineCode(size_t size) {
  // TODO: If there we have an existing machine code area, allocate
  // a new one within jump-range of any existing areas.  For now, we just
  // allocate a large area and then fail if we need more.
  
  // The exit handler must be reachable from the generated code.
  uintptr_t target = (uintptr_t)(void *)&asmExit & ~(uintptr_t)0xffff;
  const uintptr_t range = (1u << LC_TARGET_JUMPRANGE) - (1u << 21);
  uintptr_t hint = 0;
  for (int i = 0; i < 32; ++i) {
    if (isValidMachineCodePtr((void*)hint)) {
      void *p = allocMachineCodeAt(hint, size, MCPROT_GEN);
      if (isValidMachineCodePtr(p)) {
        // See if it's in range.
        if ((uintptr_t)p + size - target < range ||
            target - (uintptr_t)p < range)
          return p;
        freeMachineCode(p, size);
      }
    }

    // Try a random other address.
    do {
      hint = (0x78fb ^ prngBits(15)) << 16;  /* 64K aligned. */
    } while (!(hint + size < range));
    hint = target + hint - (range >> 1);
  }
  cerr << "FATAL: Could not allocate memory for machine code." << endl;
  return NULL;
}

void *Jit::allocMachineCodeAt(uintptr_t hint, size_t size, int prot) {
  void *p = mmap((void *)hint, size, prot, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED && !hint) {
    cerr << "Failed to allocate memory from OS." << endl;
    exit(23);
  }
  return p;
}

void Jit::freeMachineCode(void *p, size_t size) {
  munmap(p, size);
}

void Jit::protectMachineCode(void *p, size_t size, int prot) {
  mprotect(p, size, prot);
}

Fragment::Fragment()
  : flags_(0), traceId_(0), startPc_(NULL), targets_(NULL) {
}

Fragment::~Fragment() {
  if (targets_ != NULL)
    delete[] targets_;
}

_END_LAMBDACHINE_NAMESPACE
