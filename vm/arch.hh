/* Architecture-specific stuff. */
#ifndef _LAMBDACHINE_ARCH_H
#define _LAMBDACHINE_ARCH_H

#if !defined(__APPLE__)
# define _START_LAMBDACHINE_NAMESPACE namespace lambdachine {
# define _END_LAMBDACHINE_NAMESPACE   }
# define _USE_LAMBDACHINE_NAMESPACE   using namespace lambdachine;
#else
// OS X GDB cannot deal with namespaces (WTF!)
# define _START_LAMBDACHINE_NAMESPACE
# define _END_LAMBDACHINE_NAMESPACE
# define _USE_LAMBDACHINE_NAMESPACE
#endif

_START_LAMBDACHINE_NAMESPACE

namespace arch {

  typedef enum {
    ENDIAN_LITTLE,
    ENDIAN_BIG
  } Endianness;

  typedef enum {
    X86 = 1,
    X86_64,
  } Arch;

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
    const int kBitsPerWord = 32;
    const Endianness kEndianness = ENDIAN_LITTLE;
    const Arch kArch = X86;
    const char kArchName[] = "x86-32";
#elif LAMBDACHINE_TARGET == LAMBDACHINE_ARCH_X64
# define LC_ARCH_NAME		"x64"
# define LC_ARCH_BITS		64
# define LC_ARCH_ENDIAN		LAMBDACHINE_LE
# define LC_TARGET_X64		1
# define LC_TARGET_X86ORX64	1
# define LC_PAGESIZE		4096
# define LC_TARGET_JUMPRANGE	31/* +-2^31 = +-2GB */
    const int kBitsPerWord = 64;
    const Endianness kEndianness = ENDIAN_LITTLE;
    const Arch kArch = X86_64;
    const char kArchName[] = "x86-64";
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

#if defined(__WIN32__) || defined(__APPLE__)
# define NAME_PREFIX "_"
#else
# define NAME_PREFIX ""
#endif

} // end of arch namespace

_END_LAMBDACHINE_NAMESPACE

#endif
