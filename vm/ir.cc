#include "ir.hh"

_START_LAMBDACHINE_NAMESPACE

IRBuffer::IRBuffer : size_(1024) {
  realbuffer_ = new IR[size_];

  size_t nliterals = size_ / 4;
  
  bufstart_ = REF_BIAS - nliterals;
  bufend_ = bufstart + size_;

  // We want to have:
  //
  //     buffer_[REF_BIAS] = realbuffer_[nliterals];
  //
  // Thus:
  //
  //     buffer_ + REF_BIAS = realbuffer_ + nliterals
  //
  buffer_ = realbuffer_ + (nliterals - REF_BIAS);
  bufmin_ = REF_BIAS - 1;
  bufmax_ = REF_BIAS;
}

IRBuffer::~IRBuffer {
  delete[] realbuffer_;
  realbuffer_ = NULL;
  buffer_ = NULL;
}



_END_LAMBDACHINE_NAMESPACE
