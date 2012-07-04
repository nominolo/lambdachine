#ifndef _AMD64_CODEGEN_H
#define _AMD64_CODEGEN_H

#include "common.hh"
#include "jit.hh"

_START_LAMBDACHINE_NAMESPACE

class Assembler {
public:

private:
  MCode *mcp;   // Current MCode pointer (grows down).
  MCode *mclim; // Lower limit for MCode memory + red zone

  MCode *mcbot; // Bottom of reserved MCode
  MCode *mctop; // Top of generated MCode
};


_END_LAMBDACHINE_NAMESPACE

#endif /* _AMD64_CODEGEN_H */
