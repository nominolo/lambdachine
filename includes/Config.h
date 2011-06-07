#ifndef _LAMBDACHINE_CONFIG_H
#define _LAMBDACHINE_CONFIG_H

#ifndef LC_HAS_JIT
# define LC_HAS_JIT      1
#endif 

/* #define NDEBUG */

#ifndef LC_DEBUG_LEVEL
# ifdef NDEBUG
#  define LC_DEBUG_LEVEL  0
# else
#  define LC_DEBUG_LEVEL  2
# endif
#endif

#endif
