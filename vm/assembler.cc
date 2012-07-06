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

void Assembler::emit_rmro(x86Op xo, Reg rr, Reg rb, int32_t offset) {
  MCode *p = mcp;
  x86Mode mode;
  if (isReg(rb)) {
    if (offset == 0 && (rb & 7) != RID_EBP) {
      mode = XM_OFS0;
    } else if (checki8(offset)) {
      *--p = (MCode)offset;
      mode = XM_OFS8;
    } else {
      p -= 4;
      *(int32_t *)p = offset;
      mode = XM_OFS32;
    }
    if ((rb & 7) == RID_ESP)
      *--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
  } else {
    // offset = absolute address.
    *(int32_t *)(p-4) = offset;
    p[-5] = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
    p -= 5;
    rb = RID_ESP;
    mode = XM_OFS0;
  }
  mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

void Assembler::move(Reg dst, Reg src) {
  if (dst < RID_MAX_GPR) {
    emit_rr(XO_MOV, REX_64|dst, REX_64|src);
  } else { // XMM registers
    emit_rr(XO_MOVAPS, dst, src);
  }
}

void Assembler::loadi_u32(Reg dst, uint32_t i) {
  // TODO: Use xor dst, dst if i == 0.  That does change the flags, though.
  MCode *p = mcp;
  *(uint32_t *)(p - 4) = i;
  p[-5] = (MCode)(XI_MOVri + (dst & 7));
  p -= 5;
  emit_rex(p, 0, dst);
  mcp = p;
}

void Assembler::loadi_i32(Reg dst, int32_t i) {
  if (i >= 0) {
    loadi_u32(dst, i); // shortest encoding
  } else {
    MCode *p = mcp;
    *(int32_t *)(p - 4) = i;
    mcp = emit_opm(XO_MOVmi, XM_REG, REX_64, dst, p, -4);
  }
}

void Assembler::loadi_u64(Reg dst, uint64_t i) {
  if (checku32(i)) {
    loadi_u32(dst, (uint32_t)i);
  } else if (checki32(i)) {
    loadi_i32(dst, (int32_t)i);
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

void Assembler::load_u64(Reg dst, Reg base, int32_t offset) {
  if (dst < RID_MAX_GPR)
    emit_rmro(XO_MOV, REX_64|dst, base, offset);
  else
    emit_rmro(XO_MOVSD, dst, base, offset);
}

_END_LAMBDACHINE_NAMESPACE
