#ifndef _CAPABILITY_H_
#define _CAPABILITY_H_

#include "common.hh"
#include "vm.hh"
#include "memorymanager.hh"

_START_LAMBDACHINE_NAMESPACE

class Capability {
public:
  explicit Capability(MemoryManager *mm);
  ~Capability();
  inline Thread *currentThread() { return currentThread_; }

  inline void enableBytecodeTracing() { flags_ |= kTraceBytecode; }
  inline bool isEnabledBytecodeTracing() const {
    return flags_ & kTraceBytecode;
  }

  inline bool run() { return run(currentThread_); }
  // Eval given closure using current thread.
  bool eval(Thread *, Closure *);
  bool run(Thread *);
  inline Closure *staticRoots() const { return static_roots_; }
  
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
  typedef void *AsmFunction;

  MemoryManager *mm_;
  Thread *currentThread_;
  Closure *static_roots_;

  const AsmFunction *dispatch_;

  /* Pointers to the dispatch tables for various modes. */
  const AsmFunction *dispatch_normal_;
  const AsmFunction *dispatch_record_;
  const AsmFunction *dispatch_single_step_;

  static const u4 kTraceBytecode = 1 << 0;
  u4 flags_;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _CAPABILITY_H_ */
