#include "jit.hh"

_START_LAMBDACHINE_NAMESPACE

HotCounters::HotCounters(HotCount threshold)
  : threshold_(threshold) {
  for (int i = 0; i < kNumCounters; ++i) {
    counters_[i] = threshold;
  }
}

_END_LAMBDACHINE_NAMESPACE
