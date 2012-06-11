#ifndef _CAPABILITY_H_
#define _CAPABILITY_H_

#include "common.hh"
#include "vm.hh"

_START_LAMBDACHINE_NAMESPACE

class Capability {
public:
  explicit Capability();
  ~Capability();
  inline Thread *currentThread() { return currentThread_; }

  inline bool run() { return run(currentThread_); }
  bool run(Thread *);
  
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

  Thread *currentThread_;

  const AsmFunction *dispatch_;

  /* Pointers to the dispatch tables for various modes. */
  const AsmFunction *dispatch_normal_;
  const AsmFunction *dispatch_record_;
  const AsmFunction *dispatch_single_step_;

};

_END_LAMBDACHINE_NAMESPACE

#endif /* _CAPABILITY_H_ */
