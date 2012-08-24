#ifndef _CAPABILITY_H_
#define _CAPABILITY_H_

#include "common.hh"
#include "vm.hh"
#include "memorymanager.hh"
#include "jit.hh"

_START_LAMBDACHINE_NAMESPACE

typedef enum {
  kCall,
  kReturn
} BranchType;

class Capability {
public:
  explicit Capability(MemoryManager *mm);
  ~Capability();
  inline Thread *currentThread() { return currentThread_; }

  inline void enableBytecodeTracing() { flags_.set(kTraceBytecode); }
  inline bool isEnabledBytecodeTracing() const {
    return flags_.get(kTraceBytecode);
  }

  inline bool run() { return run(currentThread_); }
  // Eval given closure using current thread.
  bool eval(Thread *, Closure *);
  bool run(Thread *);
  inline Closure *staticRoots() const { return static_roots_; }
  inline bool isRecording() const {
    return flags_.get(kRecording);
  }
  inline Jit *jit() { return &jit_; }

  inline Word *traceExitHp() const { return traceExitHp_; }
  inline Word *traceExitHpLim() const { return traceExitHpLim_; }

  enum {
    STATE_INTERP,
    STATE_RECORD
  };

  // Sets interpreter state. Requires executing a SYNC instruction to
  // take effect.
  void setState(int state);

private:
  typedef enum {
    kModeInit,
    kModeRun
  } InterpMode;

  typedef enum {
    kInterpOk = 0,
    kInterpOutOfSteps,
    kInterpStackOverflow,
    kInterpUnimplemented
  } InterpExitCode;

  InterpExitCode interpMsg(InterpMode mode);
  BcIns *interpBranch(BcIns *srcPc, BcIns *dst_pc, Word *base, BranchType);
  void finishRecording();
  typedef void *AsmFunction;

  MemoryManager *mm_;
  Thread *currentThread_;
  Closure *static_roots_;

  const AsmFunction *dispatch_;

  /* Pointers to the dispatch tables for various modes. */
  const AsmFunction *dispatch_normal_;
  const AsmFunction *dispatch_record_;
  const AsmFunction *dispatch_single_step_;
  BcIns *reload_state_pc_; // used by interpBranch

  HotCounters counters_;
  Jit jit_;

  static const int kTraceBytecode = 0;
  static const int kRecording     = 1;
  Flags32 flags_;

  Word *traceExitHp_;
  Word *traceExitHpLim_;
  friend class Fragment;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _CAPABILITY_H_ */
