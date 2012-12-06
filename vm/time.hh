#ifndef _TIME_HH_
#define _TIME_HH_

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

#define TIME_RESOLUTION 1000000000
typedef uint64_t Time;

#define SecondsToTime(t) ((Time)(t) * TIME_RESOLUTION)
#define USToTime(t)      ((Time)(t) * 1000)
#define NSToTime(t)      ((Time)(t))
#define TimeToUS(t)      ((t) / 1000)
#define TimeToNS(t)      (t)

extern Time loader_time;
extern Time gc_time;
extern Time jit_time;
extern Time record_time;

void initializeTimer();
uint64_t getMonotonicNSec(void);

inline Time getProcessElapsedTime(void) {
  return NSToTime(getMonotonicNSec());
}

_END_LAMBDACHINE_NAMESPACE

#endif /* _TIME_HH_ */
