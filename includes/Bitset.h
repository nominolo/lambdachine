#ifndef _LAMBDACHINE_BITSET_H
#define _LAMBDACHINE_BITSET_H

#include "Common.h"

typedef Word Bitset;

#define BITSET_SIZE(n)   (((n) + LC_ARCH_BITS - 1) / LC_ARCH_BITS)
// Define a bitset like this:
//
//   Bitset b[BITSET_SIZE(42)];

void printBitset(Bitset b[], u4 size);

#include <string.h>
#include <stdio.h>

INLINE_HEADER void
clearBitset(Bitset b[], u4 size)
{
  memset(b, 0, sizeof(Word) * BITSET_SIZE(size));
}

INLINE_HEADER Word
getBit(Bitset b[], u4 bit)
{
  u4 idx = bit >> LC_ARCH_BITS_LOG2;
  Word mask = (Word)1 << (bit & (LC_ARCH_BITS - 1));
  return b[idx] & mask;
}

INLINE_HEADER void
setBit(Bitset b[], u4 bit)
{
  u4 idx = bit >> LC_ARCH_BITS_LOG2;
  Word mask = (Word)1 << (bit & (LC_ARCH_BITS - 1));
  b[idx] |= mask;
}

INLINE_HEADER void
clearBit(Bitset b[], u4 bit)
{
  u4 idx = bit >> LC_ARCH_BITS_LOG2;
  Word mask = (Word)1 << (bit & (LC_ARCH_BITS - 1));
  b[idx] &= ~mask;
}



#endif
