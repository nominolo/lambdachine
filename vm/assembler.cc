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

void Assembler::load_u32(Reg dst, uint32_t i) {
  // TODO: Use xor dst, dst if i == 0.  That does change the flags, though.
  MCode *p = mcp;
  *(uint32_t *)(p - 4) = i;
  p[-5] = (MCode)(XI_MOVri + (dst & 7));
  p -= 5;
  emit_rex(p, 0, dst);
  mcp = p;
}

void Assembler::load_i32(Reg dst, int32_t i) {
  if (i >= 0) {
    load_u32(dst, i); // shortest encoding
  } else {
    MCode *p = mcp;
    *(int32_t *)(p - 4) = i;
    mcp = emit_opm(XO_MOVmi, XM_REG, REX_64, dst, p, -4);
  }
}

void Assembler::load_u64(Reg dst, uint64_t i) {
  if (checku32(i)) {
    load_u32(dst, (uint32_t)i);
  } else if (checki32(i)) {
    load_i32(dst, (int32_t)i);
  } else { // Full-size 64 bit load
    MCode *p = mcp;
    *(uint64_t *)(p - 8) = i;
    p[-9] = (MCode)(XI_MOVri + (dst & 7));
    p[-10] = 0x48 + ((dst >> 3) & 1);
    p -= 10;
    mcp = p;
  }
}

void Assembler::ret() {
  *--mcp = XI_RET;
}

_END_LAMBDACHINE_NAMESPACE
