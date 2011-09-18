#ifndef _LAMBDACHINE_CONFIG_H
#define _LAMBDACHINE_CONFIG_H

#ifndef LC_HAS_JIT
# define LC_HAS_JIT      1
#endif

#ifndef LC_HAS_ASM_BACKEND
# define LC_HAS_ASM_BACKEND 1
#endif

#undef NDEBUG
#define DEBUG

#ifndef LC_DEBUG_LEVEL
# ifdef NDEBUG
#  define LC_DEBUG_LEVEL  0
# else
#  define LC_DEBUG_LEVEL  1
# endif
#endif

#endif
