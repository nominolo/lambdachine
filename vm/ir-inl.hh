#ifndef _IR_INL_H_
#define _IR_INL_H_

#include "ir.hh"
#include "assembler.hh"

_START_LAMBDACHINE_NAMESPACE

inline bool IR::hasRegOrSpill() const {
  return isReg(reg()) || spill() != 0;
}

_END_LAMBDACHINE_NAMESPACE

#endif /* _IR-INL_H_ */
