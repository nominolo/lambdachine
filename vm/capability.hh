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
  
private:
  Thread *currentThread_;
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _CAPABILITY_H_ */
