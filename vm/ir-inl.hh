#ifndef _IR_INL_H_
#define _IR_INL_H_

#include "ir.hh"
#include "assembler.hh"

_START_LAMBDACHINE_NAMESPACE

inline bool IR::hasRegOrSpill() const {
  return isReg(reg()) || spill() != 0;
}

/// Turns an unbiased buffer into a biased buffer.
///
/// A biased buffer can be accessed via an IRRef without further
/// calculations.
inline IR *biasBuffer(IR *realbuf, int maxliterals) {
  // We want to have:
  //
  //     biasedbuf[REF_BIAS] = realbuf[maxliterals];
  //
  // Thus:
  //
  //     biasedbuf + REF_BIAS = realbuf + maxliterals
  //
  //     biasedbuf = realbuf + maxliterals - REF_BIAS
  return realbuf + (maxliterals - REF_BIAS);
}

inline IR *unbiasBuffer(IR *biasedbuf, int maxliterals) {
  return biasedbuf + ((int)REF_BIAS - maxliterals);
}

_END_LAMBDACHINE_NAMESPACE

#endif /* _IR-INL_H_ */
