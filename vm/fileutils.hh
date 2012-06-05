#ifndef _FILEUTILS_H_
#define _FILEUTILS_H_

#include "common.hh"

#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>

_START_LAMBDACHINE_NAMESPACE

/* TODO: make Windows compatible */
bool fileExists(const char *path);

inline Word zigZagEncode(Word v)
{
  if (v & ((Word)1u << (sizeof(Word) * 8 - 1)))
    return (~v << 1) + 1;
  else
    return v << 1;
}

inline Word zigZagDecode(Word v)
{
  if (v & 1)
    return ~(v >> 1);
  else
    return v >> 1;
}

_END_LAMBDACHINE_NAMESPACE


#endif /* _FILEUTILS_H_ */
