#ifndef _LAMBDACHINE_FILEUTILS_H_
#define _LAMBDACHINE_FILEUTILS_H_

#include "Common.h"

#include <stdio.h>

INLINE_HEADER u1 fget_u1(FILE *f)
{
  return (u1)fgetc(f);
}

INLINE_HEADER u2 fget_u2(FILE *f)
{
  u2 hi = fget_u1(f);
  u2 lo = fget_u1(f);
  return hi << 8 | lo;
}

u4 fget_u4(FILE *f);

/* Read a length-prefixed string.  Length is a varuint. */
char *fget_string(FILE *f);

Word fget_varuint_slow(FILE *f, Word first);
/* Decode an unsigned variable length integer. */
INLINE_HEADER Word fget_varuint(FILE *f)
{
  // Optimised for common case (1 byte value)
  Word b = fget_u1(f);
  if ((b & 0x80) == 0)
    return b;
  else
    return fget_varuint_slow(f, b & 0x7f);
}


/*

ZigZag encoding
---------------

Maps negative numbers into positive numbers.

     0 -> 0
    -1 -> 1
     1 -> 2
    -2 -> 3
     2 -> 4
    -3 -> 5
      ...

In other words, the lowest bit becomes the sign bit and the remaining
bits contain the absolute value.

This is useful for varint encoding of signed numbers.  If a negative
number is encoded in twos-complement the highest bit will always be
set and a 32 bit number would require 5 bytes.  When using zigzag
encoding, the length will be proportional to the absolute value of the
number.

*/

INLINE_HEADER Word zigZagEncode(Word v)
{
  if (v & ((Word)1u << (sizeof(Word) * 8 - 1)))
    return (~v << 1) + 1;
  else
    return v << 1;
}

INLINE_HEADER Word zigZagDecode(Word v)
{
  if (v & 1)
    return ~(v >> 1);
  else
    return v >> 1;
}

INLINE_HEADER WordInt fget_varsint(FILE *f)
{
  Word val = fget_varuint(f);
  return zigZagDecode(val);
}



#endif
