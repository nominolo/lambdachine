#include "assembler.hh"
#include "jit.hh"

#include <iostream>

#define MCLIM_REDZONE	64

_START_LAMBDACHINE_NAMESPACE

using namespace std;

Assembler::Assembler(Jit *J) {
  jit_ = J;
  mctop = J->mcode()->reserve(&mcbot);
  mcend = mctop;
  mcp = mctop;
  mclim = mcbot + MCLIM_REDZONE;
  ir_ = jit_->buf_.buffer_;  // REF-biased
  buf_ = &jit_->buf_; // For looking up constants.
}

Assembler::~Assembler() {
  if (mcp != NULL) {
    jit()->mcode()->abort();
  }
  mcp = NULL;
}

void Assembler::setupRegAlloc() {
  freeset_ = kGPR;
  modset_ = RegSet();
  //  weakset_ = 
  phiset_ = RegSet();
  spill_ = 1;
  for (Reg r = RID_MIN_GPR; r < RID_MAX; r++) {
    cost_[r] = RegCost();
  }
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

void Assembler::store_u64(Reg src, Reg base, int32_t offset) {
  if (src < RID_MAX_GPR)
    emit_rmro(XO_MOVto, REX_64|src, base, offset);
  else
    emit_rmro(XO_MOVSDto, src, base, offset);
}

void Assembler::storei_u64(Reg base, int32_t offset, int32_t i) {
  emit_i32(i);
  emit_rmro(XO_MOVmi, REX_64|0, base, offset);
}

// --- Register Allocation -------------------------------------------

inline bool canRemat(IRRef ref) { return ref < REF_BIAS; }

Reg Assembler::allocRef(IRRef ref, RegSet allow) {
  IR *ins = ir(ref);
  RegSet pick = freeset_.intersect(allow);
  Reg r;
  LC_ASSERT(isNoReg(ins->reg()));

  if (!pick.isEmpty()) {
    // If we have a hint, try to use that first.
    if (hasHint(ins->reg())) {
      r = getHint(ins->reg());
      if (pick.test(r)) // Are we allowed to use this register?
        goto found;
      // Rematerialising a constant is cheaper than missing a hint.
      if (allow.test(r) && canRemat(cost_[r].ref())) {
        rematConstant(cost_[r].ref());
        goto found;
      }
      // hintmiss
    }
    // TODO: LuaJIT uses the modset for non-phi refs. I don't fully
    // understandy why, yet.  Also, if possible this code should
    // allocate callee-save regs.
    r = pick.pickBot();
  } else { // No regs available.
    r = evictReg(allow);
  }
 found:
  ins->setReg(r);
  freeset_.clear(r);
  cost_[r] = RegCost(ref, (IRType)ins->t());
  return r;
}

int32_t Assembler::spill(IR *ins) {
  int32_t slot = ins->spill();
  if (slot == 0) {
    slot = spill_;
    ++spill_;

    if (spill_ > 255) {
      cerr << "Too many spills" << endl;
      exit(14);
    }
    ins->setSpill(slot);
  }
  return slot * 8;  // FIXME: hardcoded constant
}

Reg Assembler::restoreReg(IRRef ref) {
  if (canRemat(ref)) {
    return rematConstant(ref);
  } else {
    IR *ins = ir(ref);
    int32_t ofs = spill(ins);
    Reg r = ins->reg();
    LC_ASSERT(isReg(r));
    setHint(ins, r);  // Keep hint.
    freeReg(r);
    store_u64(r, RID_BASE, ofs);
    return r;
  }
}

#define MINCOST(name) \
  if (kGPR.test(RID_##name) && /* constant-foldable */ \
      LC_LIKELY(allow.test(RID_##name)) && \
      cost_[RID_##name].raw() < cost.raw())     \
    cost = cost_[RID_##name];

Reg Assembler::evictReg(RegSet allow) {
  IRRef ref;
  RegCost cost = kMaxCost;
  LC_ASSERT(!allow.isEmpty());
  if (allow.raw() < RID_MAX_GPR) {
    // Unrolled linear search for register with smallest cost.
    GPRDEF(MINCOST);
  } else {
    exit(13);
  }
  ref = cost.ref();
  return restoreReg(ref);
}

Reg Assembler::rematConstant(IRRef ref) {
  IR *ins = ir(ref);
  Reg r = ins->reg();
  LC_ASSERT(isReg(r) && ins->spill() == 0);  // We never spill constants
  freeReg(r);
  modifiedReg(r);
  ins->setReg(RID_INIT); // No hint.
  if (ins->opcode() == IR::kKINT || ins->opcode() == IR::kKWORD) {
    uint64_t k = buf_->literalValue(ref);
    loadi_u64(r, k);
  } else {
    LC_ASSERT(ins->opcode() == IR::kKBASEO);
    int32_t ofs = ins->i32() * 8;  // TODO: Hardcoded word width.
    emit_rmro(XO_LEA, r|REX_64, RID_BASE, ofs);
  }
  return r;
}

Reg Assembler::allocDestReg(IR *ir, RegSet allow) {
  // XXX: Temporary
  return RID_EAX;
}


_END_LAMBDACHINE_NAMESPACE
