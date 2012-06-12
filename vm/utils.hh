#ifndef _UTILS_H_
#define _UTILS_H_

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

template <typename A> inline bool within(A low, A high, A value) {
  return low <= value && value <= high;
};

inline Word clearLowBits(int num_bits, Word value) {
  if (num_bits >= LC_ARCH_BITS)
    return 0;
  else
    return value & ~((1UL << num_bits) - 1);
}

/**
 * Checks that the pointer is value is a multiple of the given power
 * of 2.
 *
 * In other words, checks that the lowest {@code power} bits of {@code
 * ptr} are zero.
 */
inline bool isAlignedAtPowerOf2(int power, void *ptr) {
  Word value = reinterpret_cast<Word>(ptr);
  return (value & ((1UL << power) - 1)) == 0;
}

/**
 * Rounds the given value up to the next multiple of the given power
 * of 2.
 *
 * For example, roundUpToPowerOf2(0x7123, 12) == 0x8000 because 0x8000
 * is the next multiple of 2^12 that is >= 0x7123.
 */
inline Word roundUpToPowerOf2(int power, Word value) {
  return (value + (1UL << power) - 1) & ~((1UL << power) - 1);
}

inline bool isWordAligned(char *ptr) {
  return isAlignedAtPowerOf2(LC_ARCH_BYTES_LOG2, ptr);
}

_END_LAMBDACHINE_NAMESPACE

#endif /* _UTILS_H_ */
