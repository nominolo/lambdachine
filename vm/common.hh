#ifndef LAMBDACHINE_COMMON_H
#define LAMBDACHINE_COMMON_H

#include "arch.hh"
#include "config.hh"
#include <stdint.h>
#include <cstddef>
#include <cstdlib>
#include <cassert>
#include <cstdio>

_START_LAMBDACHINE_NAMESPACE

#if __GNUC__ >= 3
/* Assume that a flexible array member at the end of a struct
 * can be defined thus: T arr[]; */
#define FLEXIBLE_ARRAY
#else
/* Assume that it must be defined thus: T arr[0]; */
#define FLEXIBLE_ARRAY 0
#endif

/* Static (compile-time) assertions. */
#define LC_ASSERT_NAME2(name, line)    name ## line
#define LC_ASSERT_NAME(line)           LC_ASSERT_NAME2(lc_assert_, line)
#define LC_STATIC_ASSERT(cond) \
  enum { LC_ASSERT_NAME(__LINE__) = 1/!!(cond) }
/*   extern void LC_ASSERT_NAME(__LINE__)(int STATIC_ASSERTION_FAILED[(cond)?1:-1]) */

#define LC_ASSERT(expr)                assert(expr)

#if LC_ARCH_BITS == 32

typedef uint16_t HalfWord;
typedef uint32_t Word;
typedef int32_t  WordInt;
#define FMT_Word  "u"
#define FMT_WordX "x"
#define FMT_Int   "d"
#define FMT_WordLen "8"
#define LC_ARCH_BITS_LOG2 5

#elif LC_ARCH_BITS == 64

typedef uint32_t HalfWord;
typedef uint64_t Word;
typedef int64_t  WordInt;
#define FMT_Word  FMT_Word64
#define FMT_WordX FMT_Word64X
#define FMT_Int   FMT_Int64
#define FMT_WordLen "16"
#define LC_ARCH_BITS_LOG2 6
#define MARKER_UNUSED  0xfefefefe0badda7a

#else
#error "Only 32 bit and 64 bit architectures supported."
#endif

namespace arch {
#if LC_ARCH_BITS == 32
    const int kBitsPerWordLog2 = 5;
#elif LC_ARCH_BITS == 64
    const int kBitsPerWordLog2 = 6;
#endif
}


#define LC_ARCH_BYTES_LOG2 (LC_ARCH_BITS_LOG2 - 3)

/* LC_STATIC_ASSERT(sizeof(void*) == sizeof(Word)); */

typedef uint8_t  u1;
typedef uint16_t u2;
typedef uint32_t u4;
typedef uint64_t u8;

typedef int8_t   i1;
typedef int16_t  i2;
typedef int32_t  i4;
typedef int64_t  i8;

/* LC_STATIC_ASSERT(sizeof(u1) == 1); */
/* LC_STATIC_ASSERT(sizeof(u2) == 2); */
/* LC_STATIC_ASSERT(sizeof(u4) == 4); */
/* LC_STATIC_ASSERT(sizeof(u8) == 8); */

#define u4ptr(p) ((u4)(intptr_t)(void *)(p))

#define in_range_i4(w) ((i8)(i4)(w) == (i8)(w))

#if LC_ARCH_ENDIAN == LAMBDACHINE_BE
#define LC_ENDIAN_LOHI(lo, hi)  hi lo
#else
#define LC_ENDIAN_LOHI(lo, hi)  lo hi
#endif

#ifndef cast
#define cast(t, exp)    ((t)(exp))
#endif

#define cast_byte(i)	cast(u1, (i))


#define wordsof(x)  ((sizeof(x) + sizeof(Word)-1) / sizeof(Word))
#define countof(x)  (sizeof(x) / sizeof(*x))

#define byte_offset(from, to)  (cast(u1*, (to)) - cast(u1*, (from)))

static inline int32_t byteOffset32(void *from, void *to) {
  ptrdiff_t offset = cast(u1*, to) - cast(u1*, from);
  LC_ASSERT(in_range_i4(offset));
  return static_cast<int32_t>(offset);
}

// Tests whether x's size is a multiple of the size of a word.
#define WORD_ALIGNED_SIZE(x) \
  ((sizeof(x) & ((1 << LC_ARCH_BYTES_LOG2) - 1)) == 0)

#define is_word_aligned(x) \
  ((((Word)(x)) & (sizeof(Word) - 1)) == 0)

#if __GNUC__ >= 3
/* Assume that a flexible array member at the end of a struct
 * can be defined thus: T arr[]; */
#define FLEXIBLE_ARRAY
#else
/* Assume that it must be defined thus: T arr[0]; */
#define FLEXIBLE_ARRAY 0
#endif

// If multiple flags are specified, checks whether *all* flags are
// set.
#define TEST_FLAG(dst, f)  (((dst) & (f)) == (f))
#define SET_FLAG(dst, f)   ((dst) |= (f))
#define CLEAR_FLAG(dst, f) ((dst) &= ~(f))

/* Note: a and b must not have side effects. */
#define MAX(a, b) ((a) < (b) ? (b) : (a));

/*  
Inlining 
--------

INLINE_HEADER: inline functions in header files (like macros)
EXTERN_INLINE: functions that should be inlined but also be callable
from separately compiled modules.

*/
#if defined(__GNUC__) || defined( __INTEL_COMPILER)

# define INLINE_HEADER static inline
# define LC_AINLINE inline __attribute__((always_inline))
# define LC_NORET __attribute__((noreturn))
# define LC_NOINLINE __attribute__((noinline))
# define LC_USED     __attribute__((used))

// Comment from GHC's Rts.h:
// 
// The special "extern inline" behaviour is now only supported by gcc
// when _GNUC_GNU_INLINE__ is defined, and you have to use
// __attribute__((gnu_inline)).  So when we don't have this, we use
// ordinary static inline.
//
// Apple's gcc defines __GNUC_GNU_INLINE__ without providing
// gnu_inline, so we exclude MacOS X and fall through to the safe
// version.
//
#if defined(__GNUC_GNU_INLINE__) && !defined(__APPLE__)
#  if defined(KEEP_INLINES)
#    define EXTERN_INLINE inline
#  else
#    define EXTERN_INLINE extern inline __attribute__((gnu_inline))
#  endif
#else
#  if defined(KEEP_INLINES)
#    define EXTERN_INLINE
#  else
#    define EXTERN_INLINE INLINE_HEADER
#  endif
#endif

#elif defined(_MSC_VER)
# define INLINE_HEADER __inline static
# if defined(KEEP_INLINES)
#  define EXTERN_INLINE __inline
# else
#  define EXTERN_INLINE __inline extern
# endif
#else
# error "Don't know how to inline functions with your C compiler."
#endif

#define MSB_u4(hh,hl,lh,ll) \
  ((hh) << 24 | (hl) << 16 | (lh) << 8 | (ll))

#if defined(__GNUC__)

#if (__GNUC__ < 3) || ((__GNUC__ == 3) && __GNUC_MINOR__ < 4)
#error "sorry, need GCC 3.4 or newer"
#endif

#if defined(__i386__)
#define LC_FASTCALL	__attribute__((fastcall))
#else
#define LC_FASTCALL
#endif

#define LC_LIKELY(x)	__builtin_expect(!!(x), 1)
#define LC_UNLIKELY(x)	__builtin_expect(!!(x), 0)

#else

#error "Unknown compiler.  Don't know how to define LC_FASTCALL et al."

#endif
  
/* Terminal colours */

#define COL_RESET  "\033[0m"
#define COL_BLUE   "\033[34m"
#define COL_GREEN  "\033[32m"
#define COL_YELLOW "\033[33m"
#define COL_GREY   "\033[30;1m"
#define COL_RED    "\033[31m"
#define COL_PURPLE "\033[35m"
#define COL_UNDERLINE "\033[4m"

#define COLOURED(col, str)   col str COL_RESET

INLINE_HEADER void *xmalloc(size_t s)
{
  void *p = malloc(s);
  if (p == NULL) exit(1);
  return p;
}

INLINE_HEADER void xfree(void *p)
{
  free(p);
}

INLINE_HEADER void sayonara(const char *last_words) {
  fprintf(stderr, "*** FATAL: %s\nExiting...\n", last_words);
  exit(EXIT_FAILURE);
}

/* -- Debugging --------------------------------------------------- */

#ifndef NDEBUG

# ifndef LC_DEBUG_LEVEL
#  define LC_DEBUG_LEVEL 1
# endif

# define DBG_PR(...)  fprintf(stderr, __VA_ARGS__)
# define DBG_LVL(lvl, ...)  \
  do { if ((lvl) <= LC_DEBUG_LEVEL) fprintf(stderr, __VA_ARGS__); } \
  while (0)
# define IF_DBG_LVL(lvl, stmt)  if ((lvl) <= LC_DEBUG_LEVEL) { stmt; }
# define IF_DBG(stmt)            IF_DBG_LVL(0, stmt)

#else

# define DBG_PR(...)             do {} while (0)
# define DBG_LVL(lvl, ...)       do {} while (0)
# define IF_DBG_LVL(lvl, stmt)   if (0) { }
# define IF_DBG(stmt)            do {} while (0)

#endif

/* JIT compiler limits. */
#define LC_MAX_JSLOTS	250		/* Max. # of stack slots for a trace. */
#define LC_MAX_PHI	32		/* Max. # of PHIs for a loop. */
#define LC_MAX_EXITSTUBGR	8	/* Max. # of exit stub groups. */

/* Various macros. */
#define i32ptr(p)	((int32_t)(intptr_t)(void *)(p))
#define u32ptr(p)	((uint32_t)(intptr_t)(void *)(p))

#define checki8(x)	((x) == (int32_t)(int8_t)(x))
#define checku8(x)	((x) == (int32_t)(uint8_t)(x))
#define checki16(x)	((x) == (int32_t)(int16_t)(x))
#define checku16(x)	((x) == (int32_t)(uint16_t)(x))
#define checki32(x)	((x) == (int32_t)(x))
#define checku32(x)	((x) == (uint32_t)(x))
#define checkptr32(x)	((uintptr_t)(x) == (uint32_t)(uintptr_t)(x))
#define UNUSED(x)  ((void)(x))  /* to avoid warnings */
#define lc_ffs(x)  ((uint32_t)__builtin_ctz(x))       /* position of last bit */
/* postion of first bit (most significant) set */
static LC_AINLINE uint32_t lc_fls(uint32_t x)
{
  uint32_t r; __asm__("bsrl %1, %0" : "=r" (r) : "r" (x) : "cc"); return r;
}
/* swap order of bytes in x */
static LC_AINLINE uint32_t lc_bswap(uint32_t x)
{
  uint32_t r; __asm__("bswap %0" : "=r" (r) : "0" (x)); return r;
}

// For abstracting flags.
class Flags32 {
public:
  inline Flags32() : flags_(0) {}
  inline explicit Flags32(uint32_t val) : flags_(val) {}
  inline ~Flags32() {}
  inline bool get(int n) const { return flags_ & (1u << n); }
  inline void set(int n) { flags_ |= (1u << n); }
  inline void set(int n, bool value) {
    uint32_t mask = 1u << n;
    // branchless version of:
    //
    //   if (value) flags_ |= mask; else flags_ &= ~mask;
    //
    // Clang and GCC both optimise this to a branchless version, but
    // this version is a few percent faster on my machine.
    flags_ ^= (-value ^ flags_) & mask;
  }
  inline void clear(int n) { flags_ &= ~(1u << n); }
  inline void clear() { flags_ = 0; }
  inline void toggle(int n) { flags_ ^= (1u << n); }
  inline uint32_t raw() const { return flags_; }
private:
  uint32_t flags_;
};

/* A really naive Bloom filter. But sufficient for our needs. */
typedef Word BloomFilter;
#define BLOOM_MASK	(8*sizeof(BloomFilter) - 1)
#define bloombit(x)	((Word)1 << ((x) & BLOOM_MASK))
#define bloomset(b, x)	((b) |= bloombit((x)))
#define bloomtest(b, x)	((b) & bloombit((x)))

_END_LAMBDACHINE_NAMESPACE

#endif /* LAMBDACHINE_COMMON_H */
