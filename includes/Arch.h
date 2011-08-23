/* Architecture-specific stuff. */
#ifndef _LAMBDACHINE_ARCH_H
#define _LAMBDACHINE_ARCH_H

/* Target endianess. */
#define LAMBDACHINE_LE	0
#define LAMBDACHINE_BE	1

/* Target architectures. */
#define LAMBDACHINE_ARCH_X86    1
#define LAMBDACHINE_ARCH_x86    1
#define LAMBDACHINE_ARCH_X64    2
#define LAMBDACHINE_ARCH_x64    2

#ifndef LAMBDACHINE_TARGET

# if defined(__i386) || defined(__i386__) || defined(_M_IX86)
#  define LAMBDACHINE_TARGET    LAMBDACHINE_ARCH_X86
# elif defined(__x86_64__) || defined(__x86_64) || defined(_M_X64) || defined(_M_AMD64)
#  define LAMBDACHINE_TARGET    LAMBDACHINE_ARCH_X64
# else
#  error "No support for this architecture (yet)"
# endif

#endif

/* Set target properties. */
#if LAMBDACHINE_TARGET == LAMBDACHINE_ARCH_X86
# define LC_ARCH_NAME		"x86"
# define LC_ARCH_BITS		32
# define LC_ARCH_ENDIAN		LAMBDACHINE_LE
# define LC_TARGET_X86		1
# define LC_TARGET_X86ORX64	1
# define LC_PAGESIZE		4096
#elif LAMBDACHINE_TARGET == LAMBDACHINE_ARCH_X64
# define LC_ARCH_NAME		"x64"
# define LC_ARCH_BITS		64
# define LC_ARCH_ENDIAN		LAMBDACHINE_LE
# define LC_TARGET_X64		1
# define LC_TARGET_X86ORX64	1
# define LC_PAGESIZE		4096
# define LC_TARGET_JUMPRANGE	31/* +-2^31 = +-2GB */
#else
#error "No target architecture defined"
#endif

#ifdef __MINGW32__
# define FMT_Word64X    "I64x"
# define FMT_Word64     "I64u"
# define FMT_Int64      "I64d"
#elif defined(__linux__) && LC_ARCH_BITS == 64
// FIXME: need a better way to find out which format specs to use
# define FMT_Word64X    "lx"
# define FMT_Word64     "lu"
# define FMT_Int64      "ld"
#else
# define FMT_Word64X    "llx"
# define FMT_Word64     "llu"
# define FMT_Int64      "lld"
#endif

#endif
