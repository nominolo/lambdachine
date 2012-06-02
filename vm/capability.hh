#ifndef _CAPABILITY_H_
#define _CAPABILITY_H_

#include "common.hh"
#include "vm.hh"

namespace lambdachine {

class Capability {
public:
  explicit Capability();
  ~Capability();
  inline Thread *currentThread() { return currentThread_; }
  
private:
  Thread *currentThread_;
};

}

#endif /* _CAPABILITY_H_ */
