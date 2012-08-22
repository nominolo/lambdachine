#ifndef _TIME_HH_
#define _TIME_HH_

#include "common.hh"

#if HAVE_CLOCK_GETTIME
# include <time.h>
#else
# error "This architecture doesn't support `clock_gettime`."
#endif

_START_LAMBDACHINE_NAMESPACE

# ifdef _POSIX_MONOTONIC_CLOCK
#  define CLOCK_ID CLOCK_MONOTONIC
# else
#  define CLOCK_ID CLOCK_REALTIME
# endif

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

inline uint64_t getMonotonicNSec(void) {
  struct timespec ts;

  clock_gettime(CLOCK_ID, &ts);
  return (uint64_t)ts.tv_sec * 1000000000 + (uint64_t)ts.tv_nsec;
}

inline Time getProcessElapsedTime(void) {
  return NSToTime(getMonotonicNSec());
}

_END_LAMBDACHINE_NAMESPACE

#endif /* _TIME_HH_ */
