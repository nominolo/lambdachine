#ifndef _LAMBDACHINE_CONFIG_H
#define _LAMBDACHINE_CONFIG_H

#ifndef LC_HAS_JIT
# define LC_HAS_JIT      1
#endif

#ifndef LC_HAS_ASM_BACKEND
# define LC_HAS_ASM_BACKEND 1
#endif

/* #define LC_SELF_CHECK_MODE */

/* #undef NDEBUG */
/* #define DEBUG */

#ifndef LC_DEBUG_LEVEL
# ifdef NDEBUG
#  define LC_DEBUG_LEVEL  0
# else
#  define LC_DEBUG_LEVEL  1
# endif
#endif

#define DEBUG_MEMORY_MANAGER 0x00000001L
#define DEBUG_LOADER         0x00000002L
#define DEBUG_INTERPRETER    0x00000004L

#define DEBUG_COMPONENTS     0xffffffffL

#endif
