#ifndef _JIT_H_
#define _JIT_H_

#include "common.hh"
#include "vm.hh"
#include "bytecode.hh"

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
  bool recordIns(BcIns *, Word *base);

  inline bool isRecording() const { return cap_ != NULL; }

  Fragment *traceAt(BcIns *pc) {
    Word idx = reinterpret_cast<Word>(pc) >> 2;
    return fragments_[idx];
  }

  void *allocMachineCodeAt(uintptr_t hint, size_t size, int prot);
  void freeMachineCode(void *p, size_t size);
  void protectMachineCode(void *p, size_t size, int prot);

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

private:
  Fragment();
  ~Fragment();

  static const int kIsCompiled = 1;

  Flags32 flags_;
  uint32_t traceId_;
  BcIns *startPc_;
  BcIns **targets_;
  uint32_t numTargets_;


  friend class Jit;
};

extern "C" void asmEnter(Fragment *F, Thread *T, Word *spillArea,
                         Word *hp, Word *hplim, Word *stacklim, MCode *code);

_END_LAMBDACHINE_NAMESPACE

#endif /* _JIT_H_ */
