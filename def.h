#ifndef _LAMBDACHINE_DEF_H
#define _LAMBDACHINE_DEF_H

#include <stddef.h>

#include "arch.h"

#ifdef _MSC_VER
/* MSVC is stuck in the last century and doesn't have C99's stdint.h. */
typedef __int8 int8_t;
typedef __int16 int16_t;
typedef __int32 int32_t;
typedef __int64 int64_t;
typedef unsigned __int8 uint8_t;
typedef unsigned __int16 uint16_t;
typedef unsigned __int32 uint32_t;
typedef unsigned __int64 uint64_t;
# ifdef _WIN64
typedef __int64 intptr_t;
typedef unsigned __int64 uintptr_t;
# else
typedef __int32 intptr_t;
typedef unsigned __int32 uintptr_t;
# endif
#else
#include <stdint.h>
#endif

#if LC_ARCH_BITS == 32

typedef uint32_t StgWord;
typedef int32_t  StgInt;
#define FMT_WordX  "x"
#define FMT_Word   "u"
#define FMT_Int    "d"

#elif LC_ARCH_BITS == 64
typedef uint64_t StgWord;
typedef int64_t  StgInt;

#define FMT_WordX  FMT_Word64X
#define FMT_Word   FMT_Word64
#define FMT_Int    FMT_Int64

#endif

typedef StgWord *StgPtr;

#ifndef cast
#define cast(t, exp)	((t)(exp))
#endif

#define cast_byte(i)	cast(uint8_t, (i))

#endif
