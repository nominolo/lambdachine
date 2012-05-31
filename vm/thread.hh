#ifndef _THREAD_H_
#define _THREAD_H_

#include "common.hh"
#include "vm.hh"
#include "bytecode.hh"

namespace lambdachine {

class Thread {

public:
  static const Word kMinStackWords = 64;

  static Thread *createThread(Capability *, Word stackSizeInWords);
  ~Thread();

  inline BcIns *pc() { return pc_; }
  inline Word *stackStart() { return stack_; }
  inline Word *stackLimit() { return stack_ + stackSize_; }
  inline Word *top() { return top_; }
  inline Word *base() { return base_; }

  inline bool isValidSlot(int32_t n) {
    return n >= -1 && (base_ + n < top_);
  }
  bool isValid();

  inline Word slot(int32_t n) { return base_[n]; }
  inline void setSlot(int32_t n, Word value) {
    base_[n] = value;
  }

  inline Word lastResult() { return lastResult_; }
  inline void setLastResult(Word value) { lastResult_ = value; }

private:
  Thread(Word stackSizeInWords);
  void initialize();

  static BcIns stopCode_[];

  Word header_;
  BcIns *pc_;
  Word stackSize_;
  Word *base_;
  Word *top_;
  Word lastResult_;
  Word *stack_;
};

}

#endif /* _THREAD_H_ */
