#include "time.hh"

#if HAVE_CLOCK_GETTIME
# include <time.h>
#elif defined(__APPLE__)
# include <mach/mach_time.h>
#else
# error "This architecture doesn't support `clock_gettime`."
#endif

_START_LAMBDACHINE_NAMESPACE

#if HAVE_CLOCK_GETTIME
# ifdef _POSIX_MONOTONIC_CLOCK
#  define CLOCK_ID CLOCK_MONOTONIC
# else
#  define CLOCK_ID CLOCK_REALTIME
# endif
#endif

#if defined(__APPLE__)
static uint64_t timer_scaling_factor_numer = 0;
static uint64_t timer_scaling_factor_denom = 0;
#endif

void initializeTimer() {
#if defined(__APPLE__)
  mach_timebase_info_data_t info;
  (void) mach_timebase_info(&info);
  timer_scaling_factor_numer = (uint64_t)info.numer;
  timer_scaling_factor_denom = (uint64_t)info.denom;
#endif
}

uint64_t getMonotonicNSec(void) {
#if HAVE_CLOCK_GETTIME
  struct timespec ts;
  clock_gettime(CLOCK_ID, &ts);
  return (uint64_t)ts.tv_sec * 1000000000 + (uint64_t)ts.tv_nsec;
#elif defined(__APPLE__)
  uint64_t time = mach_absolute_time();
  return (time * timer_scaling_factor_numer) / timer_scaling_factor_denom;
#else
# error "Cannot implement getMonotonicNSec on this platform."
#endif
}

_END_LAMBDACHINE_NAMESPACE
