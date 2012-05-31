#include "thread.hh"
#include "utils.hh"

using namespace lambdachine;

Thread::Thread(Word stackSizeInWords)
  : header_(0), base_(NULL), top_(NULL), lastResult_(0), stack_(NULL) {
  if (stackSizeInWords < kMinStackWords) {
    stackSizeInWords = kMinStackWords;
  }
  pc_ = &stopCode_[0];
  stackSize_ = stackSizeInWords;
  initialize();
}

Thread::~Thread() {
  if (stack_ != NULL) {
    delete[] stack_;
  }
  stack_ = NULL;
  base_ = NULL;
  top_ = NULL;
}

// Used to initialize the 
BcIns Thread::stopCode_[] = { BcIns::ad(BcIns::kSTOP, 0, 0) };

void Thread::initialize() {
  stack_ = new Word[stackSize_];
  base_ = stack_;
  top_ = stack_;
}

Thread *Thread::createThread(Capability *cap, Word stackSizeInWords) {
  Thread *T = new Thread(stackSizeInWords);
  return T;
}

#include <stdio.h>

#define CHECK_VALID(expr) if(!(expr)) { fprintf(stderr,  #expr "\n"); return false;}

bool Thread::isValid() {
#ifndef NDEBUG
  CHECK_VALID(stackSize_ >= kMinStackWords);
  if (stack_ != NULL) {
    CHECK_VALID(within(stack_, stack_ + stackSize_, base_));
    CHECK_VALID(within(stack_, stack_ + stackSize_, top_));
    CHECK_VALID(top_ >= base_);
  }
  CHECK_VALID(pc_ != NULL);
#endif
  return true;
}
