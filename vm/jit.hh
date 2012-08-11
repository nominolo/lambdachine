#ifndef _JIT_H_
#define _JIT_H_

#include "common.hh"
#include "vm.hh"
#include "bytecode.hh"
#include "ir.hh"
#include "assembler.hh"
#include "objects.hh"

#include <vector>
#include HASH_MAP_H

_START_LAMBDACHINE_NAMESPACE

class HotCounters {
public:
  typedef uint16_t HotCount;
  static const Word kNumCounters = 1024; // Must be power of two.

  HotCounters(HotCount threshold);
  ~HotCounters() {}

  inline HotCount get(void *pc) const {
    return counters_[hotCountHash(pc)];
  }

  inline void set(void *pc, HotCount value) {
    counters_[hotCountHash(pc)] = value;
  }

  /// Decrement the hot counter.
  ///
  /// @return true if the counter reached the hotness threshold.
  inline bool tick(void *pc) {
    HotCount c = --counters_[hotCountHash(pc)];
    if (LC_UNLIKELY(c == 0)) {
      set(pc, threshold_);
      return true;
    } else {
      return false;
    }
  }

private:
  static inline Word hotCountHash(void *pc) {
    Word val = (Word)pc;
    return ((val >> 12) ^ (val >> 4)) & (kNumCounters - 1);
  }

  HotCount counters_[kNumCounters];
  HotCount threshold_;
};


// Very simple random number generator.
class Prng {
public:
  Prng() : state_(0x72ba83e) { }  // TODO: Make more random.
  Prng(uint32_t init) : state_(init) { }

  inline uint32_t bits(int nbits) {
    state_ = state_ * 1103515245 + 12345;
    return state_ >> (32 - nbits);
  }

private:
  uint32_t state_;
};


// Manages allocation and memory protection of the machine code area.
class MachineCode {
public:
  MachineCode(Prng *);
  ~MachineCode();

  static const size_t kAreaSize = (size_t)1 << 19; // 512KB

  /// Reserve the whole machine code area.  No code from the machine
  /// code area may be running at the same time.  (It will trigger a
  /// page fault.)
  ///
  /// @param limit Lower limit of the machine code area.
  /// @return Upper limit of the machine code area.
  MCode *reserve(MCode **limit);

  // Finish generation of machine code.
  //
  // @param top E
  void commit(MCode *top);
  void commitStub(MCode *bot);
  void abort();

  // Synchronise data and instruction cache.
  void syncCache(void *start, void *end);

  /// Compiled fragment code is in the range. [start()-end()]
  inline MCode *start() const { return top_; }
  inline MCode *end() const { return (MCode*)((char*)area_ + size_); }

  /// Stub code is in the range [stubStart()-stubEnd()].
  ///
  /// INVARIANT: stubEnd() < start().
  inline MCode *stubStart() const { return area_; }
  inline MCode *stubEnd() const { return bottom_; }

  void dumpAsm(std::ostream &out);
  void dumpAsm(std::ostream &out, MCode *from, MCode *to);

private:
  void *alloc(size_t size);
  void free(void *p, size_t size);
  void *allocAt(uintptr_t hint, size_t size, int prot);
  void setProtection(void *p, size_t size, int prot);
  void allocArea();
  void protect(int prot);

  Prng *prng_;
  int protection_;
  MCode *area_;
  MCode *top_;
  MCode *bottom_;
  size_t size_;
  size_t sizeTotal_;
};


// Forward declarations.
class Capability;
class Fragment;

#define FRAGMENT_MAP \
  HASH_NAMESPACE::HASH_MAP_CLASS<Word,Fragment*>

class Jit {
public:
  Jit();
  ~Jit();

  void beginRecording(Capability *, BcIns *startPc, Word *base,
                      bool isReturn);

  // Returns true if recording finished
  bool recordIns(BcIns *, Word *base, const Code *);

  inline bool isRecording() const { return cap_ != NULL; }

  Fragment *traceAt(BcIns *pc) {
    Word idx = reinterpret_cast<Word>(pc) >> 2;
    return fragments_[idx];
  }

  inline MachineCode *mcode() { return &mcode_; }
  inline IRBuffer *buffer() { return &buf_; }
  inline Assembler *assembler() { return &asm_; }

  Fragment *saveFragment();
  Fragment *lookupFragment(BcIns *pc) {
    Word idx = reinterpret_cast<Word>(pc) >> 2;
    return fragments_[idx];
  }

private:
  void finishRecording();
  void resetRecorderState();
  void registerFragment(BcIns *startPc, Fragment *F) {
    Word idx = reinterpret_cast<Word>(startPc) >> 2;
    fragments_[idx] = F;
  }
  
  static const int kLastInsWasBranch = 0;
  static const int kIsReturnTrace = 1;

  Capability *cap_;
  BcIns *startPc_;
  Word *startBase_;
  Flags32 flags_;
  std::vector<BcIns*> targets_;
  FRAGMENT_MAP fragments_;
  Prng prng_;
  MachineCode mcode_;
  IRBuffer buf_;
  Assembler asm_;
  MCode *exitStubGroup_[16];

  void genCode(IRBuffer *buf);
  void genCode(IRBuffer *buf, IR *ir);

  friend class Assembler;
};

typedef uint32_t ExitNo;
typedef struct _ExitState ExitState; // architecture-specific.

class Fragment {
public:
  inline uint32_t traceId() const { return traceId_; }
  inline bool isCompiled() const { return flags_.get(kIsCompiled); }
  inline bool isSimulated() const { return !flags_.get(kIsCompiled); }

  // Not sure if we really need to store targets in the final trace.
  // They're really only useful for loop truncation.
  inline uint32_t targetCount() const { return numTargets_; }
  inline BcIns *target(uint32_t n) const { return targets_[n]; }

  /// Maximum amount of stack space needed by this fragment. This is
  /// the sum of all spill slots and the highest snapshot write.
  /// 
  /// TODO: What if a side trace needs to increase this value?
  inline uint16_t frameSize() const { return frameSize_; }
  inline int32_t spillOffset() const { return spillOffset_; }
  
  inline MCode *entry() { return mcode_; }
  uint64_t literalValue(IRRef, Word* base);
  void restoreSnapshot(ExitNo, ExitState *);

  ~Fragment();

private:
  Fragment();

  inline IR *ir(IRRef ref) { return &buffer_[ref]; }
  inline Snapshot &snap(SnapNo n) {
    LC_ASSERT(n < nsnaps_);
    return snaps_[n];
  }

  static const int kIsCompiled = 1;

  Flags32 flags_;
  uint32_t traceId_;
  BcIns *startPc_;

  BcIns **targets_;
  uint32_t numTargets_;
  
  IRRef firstconstant_;  // Lowest IR constant. Biased with REF_BIAS
  IRRef nextins_;        // Next IR instruction. Biased with REF_BIAS
  IR *buffer_;           // Biased buffer
  
  uint16_t frameSize_;
  uint16_t nsnaps_;
  int32_t spillOffset_;
  Snapshot *snaps_;      
  SnapshotData snapmap_;
  
  MCode *mcode_;
  //  size_t sizemcode_;

  friend class Jit;
};

/* This definition must match the asmEnter/asmExit functions */
struct _ExitState {
  double   fpr[RID_NUM_FPR];    /* Floating-point registers. */
  Word     gpr[RID_NUM_GPR];    /* General-purpose registers. */
  Word     *hplim;              /* Heap Limit */
  Word     *stacklim;           /* Stack Limit */
  Word     *spill;              /* Spill slots. */
  Thread   *T;                  /* Currently executing thread */
  Fragment *F;                  /* Fragment under execution */
};

extern "C" void asmEnter(Fragment *F, Thread *T, Word *spillArea,
                         Word *hp, Word *hplim, Word *stacklim, MCode *code);

extern "C" void asmExit(int);

_END_LAMBDACHINE_NAMESPACE

#endif /* _JIT_H_ */
