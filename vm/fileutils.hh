#ifndef _FILEUTILS_H_
#define _FILEUTILS_H_

#include "common.hh"

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>

_START_LAMBDACHINE_NAMESPACE

/* TODO: make Windows compatible */
bool fileExists(const char *path);

inline uint8_t fget_u1(FILE *f)
{
  return (uint8_t)fgetc(f);
}

inline uint16_t fget_u2(FILE *f)
{
  uint16_t hi = fget_u1(f);
  uint16_t lo = fget_u1(f);
  return hi << 8 | lo;
}

u4 fget_u4(FILE *f);

Word fget_varuint_slow(FILE *f, Word first);

/* Decode an unsigned variable length integer. */
inline Word fget_varuint(FILE *f)
{
  // Optimised for common case (1 byte value)
  Word b = fget_u1(f);
  if ((b & 0x80) == 0)
    return b;
  else
    return fget_varuint_slow(f, b & 0x7f);
}

_END_LAMBDACHINE_NAMESPACE


#endif /* _FILEUTILS_H_ */
