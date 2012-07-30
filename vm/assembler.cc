#include "assembler.hh"
#include "jit.hh"

#include <iostream>

#define MCLIM_REDZONE	64

_START_LAMBDACHINE_NAMESPACE

using namespace std;

Assembler::Assembler(Jit *J) {
  jit_ = J;
  mctop = mcend = mcp = mclim = NULL;
  ir_ = NULL;
  buf_ = NULL;
}

Assembler::~Assembler() {
  if (mcp != NULL) {
    jit()->mcode()->abort();
  }
  mcp = NULL;
}

void Assembler::setupMachineCode(MachineCode *mcode) {
  mctop = mcode->reserve(&mcbot);
  mcend = mctop;
  mcp = mctop;
  mclim = mcbot + MCLIM_REDZONE;
}

void Assembler::setupRegAlloc() {
  freeset_ = kGPR;
  modset_ = RegSet();
  //  weakset_ = 
  phiset_ = RegSet();

  for (Reg r = RID_MIN_GPR; r < RID_MAX; r++) {
    cost_[r] = RegCost();
  }
}

#define REGSP(r, s)		((r) + ((s) << 8))
#define REGSP_HINT(r)		((r)|RID_NONE)
#define REGSP_INIT		REGSP(RID_INIT, 0)

void Assembler::setup(IRBuffer *buf) {
  setupRegAlloc();

  ir_ = buf->buffer_;  // REF-biased
  buf_ = buf; // For looking up constants.

  spill_ = 1;
  curins_ = buf->bufmax_;
  nins_ = buf->bufmax_;

  // Initialise reg/spill fields for constants.
  for (IRRef i = buf->bufmin_; i < REF_BIAS; ++i) {
    ir(i)->setPrev(REGSP_INIT);
  }

  // The base reg gets a dedicated register.  Since this register is
  // not included in the allocateable registers, this will always
  // succeed.
  ir(REF_BASE)->setPrev(REGSP_HINT(RID_BASE));

  // Initialise ref/spill fields for regular instructions.
  for (IRRef i = REF_FIRST; i < nins_; ++i) {
    IR *ins = ir(i);
    // TODO: For bit shifting operations we have force non-constant
    // arguments to be allocated into ecx.
    ins->setPrev(REGSP_INIT);
  }
}

MCode *Assembler::finish() {
  MCode *top = mcp;
  jit()->mcode()->commit(top);
  jit()->mcode()->syncCache(top, mcend);
  mcp = mctop = mcend = mclim = NULL;
  return top;
}

/* op + modrm */
#define emit_opm(xo, mode, rr, rb, p, delta) \
  (p[(delta)-1] = MODRM((mode), (rr), (rb)), \
   emit_op((xo), (rr), (rb), 0, (p), (delta)))

/* op + modrm + sib */
#define emit_opmx(xo, mode, scale, rr, rb, rx, p) \
  (p[-1] = MODRM((scale), (rx), (rb)), \
   p[-2] = MODRM((mode), (rr), RID_ESP), \
   emit_op((xo), (rr), (rb), (rx), (p), -1))

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

void Assembler::emit_mrm(x86Op xo, Reg rr, Reg rb) {
  MCode *p = mcp;
  x86Mode mode = XM_REG;
  if (rb == RID_MRM) {
    rb = mrm_.base;
    if (rb == RID_NONE) {
      rb = RID_EBP;
      mode = XM_OFS0;
      p -= 4;
      *(int32_t *)p = mrm_.ofs;
      if (mrm_.idx != RID_NONE)
	goto mrmidx;
#if 1 /* LJ_64 */
      *--p = MODRM(XM_SCALE1, RID_ESP, RID_EBP);
      rb = RID_ESP;
#endif
    } else {
      if (mrm_.ofs == 0 && (rb&7) != RID_EBP) {
	mode = XM_OFS0;
      } else if (checki8(mrm_.ofs)) {
	*--p = (MCode)mrm_.ofs;
	mode = XM_OFS8;
      } else {
	p -= 4;
	*(int32_t *)p = mrm_.ofs;
	mode = XM_OFS32;
      }
      if (mrm_.idx != RID_NONE) {
      mrmidx:
	mcp = emit_opmx(xo, mode, mrm_.scale, rr, rb, mrm_.idx, p);
	return;
      }
      if ((rb&7) == RID_ESP)
	*--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
    }
  }
  mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

/* op r, i */
void Assembler::emit_gri(x86Group xg, Reg rb, int32_t i)
{
  MCode *p = mcp;
  x86Op xo;
  if (checki8(i)) {
    *--p = (MCode)i;
    xo = XG_TOXOi8(xg);
  } else {
    p -= 4;
    *(int32_t *)p = i;
    xo = XG_TOXOi(xg);
  }
  mcp = emit_opm(xo, XM_REG, (Reg)(xg & 7) | (rb & REX_64), rb, p, 0);
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

Reg Assembler::alloc1(IRRef ref, RegSet allow) {
  Reg r = ir(ref)->reg();
  if (isNoReg(r)) r = allocRef(ref, allow);
  return r;
}

Reg Assembler::destReg(IR *ins, RegSet allow) {
  Reg dest = ins->reg();
  if (isReg(dest)) {
    freeReg(dest);
    modifiedReg(dest);
  } else {
    if (hasHint(dest) && freeset_.intersect(allow).test(getHint(dest))) {
      dest = getHint(dest);
      modifiedReg(dest);
    } else {
      dest = allocScratchReg(allow);
    }
    ins->setReg(dest);
  }
  if (LC_UNLIKELY(ins->spill() != 0))
    saveReg(ins, dest);
  return dest;
}

void Assembler::saveReg(IR *ins, Reg r) {
  store_u64(r, RID_BASE, 8 * ins->spill());
}

Reg Assembler::allocScratchReg(RegSet allow) {
  Reg r = pickReg(allow);
  modifiedReg(r);
  return r;
}

Reg Assembler::pickReg(RegSet allow) {
  RegSet pick = freeset_.intersect(allow);
  if (pick.isEmpty()) {
    return evictReg(allow);
  } else {
    return pick.pickTop();
  }
}

/// Ensure that the value of lref is in register dest.
void Assembler::allocLeft(Reg dest, IRRef lref) {
  IR *ins = ir(lref);
  Reg left = ins->reg();
  if (!isReg(left)) {
    if (irref_islit(lref)) {
      ins->setReg(dest);
      rematConstant(lref);
      return;
    }
    if (!hasHint(left))
      setHint(ins, dest);  // Propagate register hint.
    
    LC_ASSERT(dest < RID_MAX_GPR); // FIXME: FP support.
    left = allocRef(lref, kGPR);
  }
  if (dest != left) {
    // TODO: Special PHI stuff here?
    move(dest, left);
  }
}

bool Assembler::swapOperands(IR *ins) {
  IR *insleft = ir(ins->op1());
  IR *insright = ir(ins->op2());
  LC_ASSERT(!isReg(insright->reg()));
  if (!IR::isCommutative(ins->opcode()))
    return false;
  if (irref_islit(ins->op2()))
    return false;  // Don't swap constants to the left.
  if (isReg(insleft->reg())) // FIXME: Why?
    return true;
  if (sameHint(insleft->reg(), insright->reg()))
    return true;
  // TODO: There's more. See LuaJIT source.
  return false;
}

/*
Reg Assembler::allocConst(IRRef ref, RegSet allow) {
  LC_ASSERT(irref_islit(ref));
  Reg r = RID_NONE;
  uint64_t k = buf_->literalValue(ref);
  if (!check_i32(k)) {
    r = allocScratchReg(allow);
    ir(ref)->setReg(r);
  }
  return r;
}
*/

/// Fuse 
Reg Assembler::fuseLoad(IRRef ref, RegSet allow) {
  IR *ins = ir(ref);
  if (isReg(ins->reg())) {
    if (!allow.isEmpty()) {  // Fast path.
      return ins->reg();
    }
    // Otherwise, access operand directly from memory.
    mrm_.base = RID_ESP;
    mrm_.ofs = spill(ins);
    mrm_.idx = RID_NONE;
    return RID_MRM;
  }
  // TODO: Do the actual fusing.
  return allocRef(ref, allow);
}

bool Assembler::is32BitLiteral(IRRef ref, int32_t *k) {
  if (irref_islit(ref)) {
    uint64_t k64 = buf_->literalValue(ref);
    if (checki32(k64)) {
      *k = (int32_t)k64;
      return true;
    }
  }
  return false;
}

void Assembler::intArith(IR *ins, x86Arith xa) {
  RegSet allow = kGPR;
  int32_t k = 0;
  IRRef lref = ins->op1();
  IRRef rref = ins->op2();
  
  Reg right = ir(rref)->reg();
  if (isReg(right)) {
    allow.clear(right);
  }
  Reg dest = destReg(ins, allow);

  if (!isReg(right) && !is32BitLiteral(rref, &k)) {
    allow.clear(dest);
    right = fuseLoad(rref, allow);
  }

  if (xa != XOg_X_IMUL) {
    if (isReg(right)) {
      emit_mrm(XO_ARITH(xa), REX_64|dest, right);
    } else {
      emit_gri(XG_ARITHi(xa), REX_64|dest, k);
    }
  } else {
    cerr << "NYI: IMUL" << endl;
    exit(4);
  }
  allocLeft(dest, lref);
}

void Assembler::assemble(IRBuffer *buf, MachineCode *mcode) {
  setup(buf);
  setupMachineCode(mcode);

  curins_ = nins_;
  IRRef stopins = REF_BASE;
  for (curins_--; curins_ > stopins; curins_--) {
    IR *ins = ir(curins_);
    if (ins->isGuard()) {
      cerr << "TODO: Add snapshot" << endl;
    }
    emit(ins);
  }

  buf->setRegsAllocated();
  // TODO: Save to trace fragment
  LC_ASSERT(freeset_.raw() == kGPR.raw());
  mcode->commit(mcp);
}

void Assembler::emitSLOAD(IR *ins) {
  int32_t ofs = 8 * (int16_t)ins->op1();
  Reg base = RID_BASE;
  RegSet allow = kGPR;
  Reg dst = destReg(ins, allow);
  load_u64(dst, base, ofs);
}

void Assembler::emit(IR *ins) {
  switch (ins->opcode()) {
  case IR::kSLOAD: emitSLOAD(ins); break;
  case IR::kADD:   intArith(ins, XOg_ADD); break;
  default:
    cerr << "NYI: codegen for ";
    ins->debugPrint(cerr, REF_BIAS + (IRRef1)(ins - ir_));
    cerr << endl;
    exit(23);
  }
}

const char *regNames32[RID_NUM_GPR] = {
  "eax", "ecx", "edx", "ebx", "esp", "ebp", "esi", "edi",
  "r8d", "r9d", "r10d", "r11d", "r12d", "r13d", "r14d", "r15d"
};

const char *regNames64[RID_NUM_GPR] = {
  "rax", "rcx", "rdx", "rbx", "rsp", "rbp", "rsi", "rdi",
  "r8", "r9", "r10", "r11", "r12", "r13", "r14", "r15"
};

const char *fpRegNames[RID_NUM_FPR] = {
  "xmm0", "xmm1", "xmm2", "xmm3", "xmm4", "xmm5", "xmm6", "xmm7", 
  "xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13", "xmm14", "xmm15"
};

_END_LAMBDACHINE_NAMESPACE
