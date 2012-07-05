#include "assembler.hh"

#define MCLIM_REDZONE	64

_START_LAMBDACHINE_NAMESPACE

Assembler::Assembler(Jit *J) {
  jit_ = J;
  mctop = J->mcode()->reserve(&mcbot);
  mcend = mctop;
  mcp = mctop;
  mclim = mcbot + MCLIM_REDZONE;
}

Assembler::~Assembler() {
  if (mcp != NULL) {
    jit()->mcode()->abort();
  }
  mcp = NULL;
}

MCode *Assembler::finish() {
  MCode *top = mcp;
  jit()->mcode()->commit(top);
  jit()->mcode()->syncCache(top, mcend);
  mcp = mctop = mcend = mclim = NULL;
  return top;
}

void Assembler::move(Reg dst, Reg src) {
  if (dst < RID_MAX_GPR) {
    emit_rr(XO_MOV, REX_64|dst, REX_64|src);
  } else { // XMM registers
    emit_rr(XO_MOVAPS, dst, src);
  }
}

void Assembler::ret() {
  *--mcp = XI_RET;
}

_END_LAMBDACHINE_NAMESPACE
