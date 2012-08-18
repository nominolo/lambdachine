#include "assembler.hh"
#include "jit.hh"
#include "ir-inl.hh"

#include <iostream>
#include <fstream>

#define MCLIM_REDZONE 64

#include "assembler-debug.hh"

_START_LAMBDACHINE_NAMESPACE

using namespace std;

static inline int32_t jmprel(MCode *p, MCode *target) {
  ptrdiff_t delta = target - p;
  LC_ASSERT(delta == (int32_t)delta);
  return (int32_t)delta;
}

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

#define REGSP(r, s)   ((r) + ((s) << 8))
#define REGSP_HINT(r)   ((r)|RID_NONE)
#define REGSP_INIT    REGSP(RID_INIT, 0)

// NOTE: This operation is NOT idempotent!  We assume that
// the prev() fields of the input instructions are valid.
// This won't be the case after this function finishes.
void Assembler::setup(IRBuffer *buf) {
  buf->setHeapOffsets();
  setupRegAlloc();

  ir_ = buf->buffer_;  // REF-biased
  buf_ = buf; // For looking up constants.

  spillOffset_ = buf->slots_.highestSlot();
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
    *(int32_t *)(p - 4) = offset;
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
      if (mrm_.ofs == 0 && (rb & 7) != RID_EBP) {
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
      if ((rb & 7) == RID_ESP)
        *--p = MODRM(XM_SCALE1, RID_ESP, RID_ESP);
    }
  }
  mcp = emit_opm(xo, mode, rr, rb, p, 0);
}

/* op r, i */
void Assembler::emit_gri(x86Group xg, Reg rb, int32_t i) {
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

/* op rm/mrm, i */
void Assembler::emit_gmrmi(x86Group xg, Reg rb, int32_t i) {
  x86Op xo;
  if (checki8(i)) {
    emit_i8(i);
    xo = XG_TOXOi8(xg);
  } else {
    emit_i32(i);
    xo = XG_TOXOi(xg);
  }
  emit_mrm(xo, (Reg)(xg & 7) | (rb & REX_64), (rb & ~REX_64));
}


void Assembler::move(Reg dst, Reg src) {
  if (dst < RID_MAX_GPR) {
    emit_rr(XO_MOV, REX_64 | dst, REX_64 | src);
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
    emit_rmro(XO_MOV, REX_64 | dst, base, offset);
  else
    emit_rmro(XO_MOVSD, dst, base, offset);
}

void Assembler::store_u64(Reg base, int32_t offset, Reg src) {
  if (src < RID_MAX_GPR)
    emit_rmro(XO_MOVto, REX_64 | src, base, offset);
  else
    emit_rmro(XO_MOVSDto, src, base, offset);
}

void Assembler::storei_u64(Reg base, int32_t offset, int32_t i) {
  emit_i32(i);
  emit_rmro(XO_MOVmi, REX_64 | 0, base, offset);
}

// --- Register Allocation -------------------------------------------

inline bool canRemat(IRRef ref) {
  return ref < REF_BIAS;
}

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
  RA_DBGX((this, "alloc     $f $r", ref, r));
  ins->setReg(r);
  freeset_.clear(r);
  cost_[r] = RegCost(ref, (IRType)ins->t());
  return r;
}

inline int32_t Assembler::spillOffset(uint8_t spillSlot) const {
  // spillOffset_ points to the highest written slot.  Hence,
  // spillOffset_[0] most not be written to.  However, spillSlots
  // start at 1, not 0, so that's all fine.
  return sizeof(Word) * (spillOffset_ + spillSlot);
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
  return spillOffset(slot);
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
    RA_DBGX((this, "restore   $i $r", ins, r));
    load_u64(r, RID_BASE, ofs);
    //    store_u64(RID_BASE, ofs, r);
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
  if (allow.raw() < (1 << RID_MAX_GPR)) {
    // Unrolled linear search for register with smallest cost.
    GPRDEF(MINCOST);
  } else {
    cerr << "Trying to evict FP regs. (allow="
         << hex << allow.raw() << ", max_gpr=" << RID_MAX_GPR << ')' << endl;
    exit(13);
  }
  ref = cost.ref();
  return restoreReg(ref);
}

void Assembler::evictConstants() {
  RegSet work = freeset_.complement().intersect(kGPR);
  while (!work.isEmpty()) {
    Reg r = work.pickBot();
    IRRef ref = cost_[r].ref();
    if (canRemat(ref) && irref_islit(ref)) {
      rematConstant(ref);
    }
    work.clear(r);
  }
}

Reg Assembler::rematConstant(IRRef ref) {
  IR *ins = ir(ref);
  Reg r = ins->reg();
  LC_ASSERT(isReg(r) && ins->spill() == 0);  // We never spill constants
  freeReg(r);
  modifiedReg(r);
  ins->setReg(RID_INIT); // No hint.
  RA_DBGX((this, "remat     $i $r", ins, r));

  if (ins->opcode() == IR::kKINT || ins->opcode() == IR::kKWORD) {
    uint64_t k = buf_->literalValue(ref);
    loadi_u64(r, k);
  } else {
    LC_ASSERT(ins->opcode() == IR::kKBASEO);
    int32_t ofs = ins->i32() * 8;  // TODO: Hardcoded word width.
    emit_rmro(XO_LEA, r | REX_64, RID_BASE, ofs);
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
  RA_DBGX((this, "dest           $r", dest));
  if (LC_UNLIKELY(ins->spill() != 0))
    saveReg(ins, dest);
  return dest;
}

void Assembler::saveReg(IR *ins, Reg r) {
  RA_DBGX((this, "save      $i $r", ins, r));
  store_u64(RID_BASE, spillOffset(ins->spill()), r);
}

Reg Assembler::allocScratchReg(RegSet allow) {
  Reg r = pickReg(allow);
  modifiedReg(r);
  return r;
}

void Assembler::evictSet(RegSet drop) {
  RegSet work;
  work = drop.intersect(freeset_.complement()).intersect(kGPR);
  while (!work.isEmpty()) {
    Reg r = work.pickBot();
    restoreReg(cost_[r].ref());
    work.clear(r);
  }
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
Reg Assembler::allocConst(IRRef ref, RegSet allow, int32_t *k) {
  LC_ASSERT(irref_islit(ref));
  if (is32BitLiteral(ref, k))
    return RID_NONE;
  else
    return allocRef(ref, allow);
}
*/

/// Fuse an operand into a memory reference or immediate, if possible.
Reg Assembler::fuseLoad(IRRef ref, RegSet allow) {
  IR *ins = ir(ref);
  if (isReg(ins->reg())) {
    if (!allow.isEmpty()) {  // Fast path.
      return ins->reg();
    }
    // Only memory operands are allowed. Access operand directly from
    // memory.
    mrm_.base = RID_ESP;
    mrm_.ofs = spill(ins);
    mrm_.idx = RID_NONE;
    return RID_MRM;
  }
  // TODO: Do the actual fusing.
  return allocRef(ref, allow);
}

bool Assembler::is32BitLiteral(IRRef ref, int32_t *k) {
  if (irref_islit(ref) && ir(ref)->opcode() != IR::kKBASEO) {
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
      emit_mrm(XO_ARITH(xa), REX_64 | dest, right);
    } else {
      emit_gri(XG_ARITHi(xa), REX_64 | dest, k);
    }
  } else {
    cerr << "NYI: IMUL" << endl;
    exit(4);
  }
  allocLeft(dest, lref);
}

void Assembler::divmod(IR *ins, DivModOp op, bool useSigned) {
  if (!useSigned) {
    cerr << "NYI: Unsigned DIV/MOD." << endl;
    exit(3);
  }

  // Intel's DIV/IDIV expects its first argument in rdx:rax:
  //
  //     reg <- right (anything but rdx,rax)
  //     rax <- left
  //     cwdq  -- sign-extend rax into rdx
  //     idiv reg
  //     mov dest, rax (DIV) -or-  mov dest, rdx (MOD)
  //
  
  Reg resultReg = (op == DIVMOD_DIV) ? RID_EAX : RID_EDX;
  Reg otherReg = (op == DIVMOD_DIV) ? RID_EDX : RID_EAX;

  RegSet allow = kGPR.exclude(RID_EAX).exclude(RID_EDX);

  // TODO: We could add hinting to the output of DIV/MOD instructions.
  // That may avoid a few spills.

  evictSet(RegSet::fromReg(otherReg));

  Reg dest = destReg(ins, allow.include(resultReg));
  if (dest != resultReg) {
    evictSet(RegSet::fromReg(resultReg));
    move(dest, resultReg);
  }
  // At this point RID_EAX/EDX are free. For each register, we either
  // we evicted it, or it was the output of this instruction.

  // Alloc1 ignores the allowed range if the ref already has a
  // register assigned.  However, we know that at this point, no
  // (other) instruction has RID_EAX or RID_EDX allocated.
  
  Reg right = alloc1(ins->op2(), allow);
  LC_ASSERT(right != RID_EAX && right != RID_EDX);

  x86Group3 xg = useSigned ? XOg_IDIV : XOg_DIV;
  emit_rr(XO_GROUP3, xg | REX_64, right);
  if (useSigned) {
    emit_i8(XI_CDQ);  // sign-extend rax into rdx:rax
    emit_i8(0x48);  // REX.W, force 64-bit variant
  } else {
    // TODO: Is it safe to use XOR here? It sets the flags.
    loadi_i32(REX_64 | RID_EDX, 0);
  }
  allocLeft(RID_EAX, ins->op1());
}

void Assembler::prepareTail(IRBuffer *buf) {
  MCode *p = mctop;
  IRRef saveref = buf->chain_[IR::kSAVE];
  if (saveref && ir(saveref)->op1()) {
    // The SAVE instruction loops back somewhere.  Reserve space for
    // the JMP instruction.
    p -= 5;
  }
  if (jit()->flags_.get(Jit::kDebugTrace)) {
    // Reserve space for CALL to asmTrace
    p -= 5;
  }
  mcp = p;
}

void Assembler::fixupTail(MCode *target) {
  MCode *p = mctop;
  if (target != NULL) {
    *(int32_t *)(p - 4) = jmprel(p, target);
    p[-5] = XI_JMP;
    p -= 5;
  }
  if (jit()->flags_.get(Jit::kDebugTrace)) {
    *(int32_t *)(p - 4) = jmprel(p, (MCode*)(void*)&asmTrace);
    p[-5] = XI_CALL;
  }
}

void Assembler::assemble(IRBuffer *buf, MachineCode *mcode) {
  RA_DBG_START();

  setup(buf);
  setupMachineCode(mcode);
  setupExitStubs(buf->numSnapshots(), mcode);

  curins_ = nins_;
  IRRef stopins = REF_BASE;
  snapno_ = buf->numSnapshots() - 1;

  prepareTail(buf);

  for (curins_--; curins_ > stopins; curins_--) {
    IR *ins = ir(curins_);
    if (ins->isGuard()) {
      Snapshot &snap = buf->snap(snapno_);
      if (snap.ref() != curins_) {
        cout << "snap.ref = " << snap.ref() - REF_BIAS
             << " curins_ = " << curins_ - REF_BIAS << endl;
      }
      LC_ASSERT(snap.ref() == curins_);
      if (ins->opcode() != IR::kSAVE)
        snapshotAlloc(snap, buf->snapmap());
      emit(ins);
      --snapno_;
    } else
      emit(ins);
  }

  evictConstants();

  MCode *target = NULL;
  IRRef saveref = buf->chain_[IR::kSAVE];
  if (saveref && ir(saveref)->op1()) {
    target = mcp;
  }
  fixupTail(target);

  buf->setRegsAllocated();
  // TODO: Save to trace fragment
  LC_ASSERT(freeset_.raw() == kGPR.raw());
  mcode->commit(mcp);

  RA_DBG_FLUSH();
}

void Assembler::emitSLOAD(IR *ins) {
  int32_t ofs = 8 * (int16_t)ins->op1();
  Reg base = RID_BASE;
  RegSet allow = kGPR;
  Reg dst = destReg(ins, allow);
  load_u64(dst, base, ofs);
}

void Assembler::itblGuard(IR *ins) {
  // Emit a guard for an info table. On x86 `EQINFO x kinfo` compiles
  // to the following two instructions:
  //
  //     cmp [x], kinfo
  //     jne _exit<N>
  //
  IRRef closref = ins->op1();
  IRRef itblref = ins->op2();
  LC_ASSERT(irref_islit(itblref));
  Reg closreg = alloc1(closref, kGPR);
  int32_t imm = 0;

  guardcc(CC_NE);
  LC_ASSERT(closreg != RID_ESP && closreg != RID_EBP);

  if (is32BitLiteral(itblref, &imm)) {
    mrm_.base = closreg;
    mrm_.ofs = 0;
    mrm_.idx = RID_NONE;
    emit_gmrmi(XG_ARITHi(XOg_CMP), RID_MRM | REX_64, imm);
  } else {
    Reg right = alloc1(itblref, kGPR);
    mrm_.base = closreg;
    mrm_.ofs = 0;
    mrm_.idx = RID_NONE;
    emit_mrm(XO_CMP, right | REX_64, RID_MRM);
  }
}

#define COMPFLAGS(cs, cu)  ((cs)+((cu)<<4))
static const uint16_t asm_compmap[IR::kNE - IR::kLT + 1] = {
  /*                signed, unsigned */
  /* LT */ COMPFLAGS(CC_GE,  CC_AE),
  /* GE */ COMPFLAGS(CC_L,   CC_B),
  /* LE */ COMPFLAGS(CC_G,   CC_A),
  /* GT */ COMPFLAGS(CC_LE,  CC_BE),
  /* EQ */ COMPFLAGS(CC_NE,  CC_NE),
  /* NE */ COMPFLAGS(CC_E,   CC_E)
};

void Assembler::guardcc(int cc) {
  MCode *target = exitstubAddr(snapno_);
  MCode *p = mcp;
  *(int32_t *)(p - 4) = jmprel(p, target);
  p[-5] = (MCode)(XI_JCCn + (cc & 15));
  p[-6] = 0x0f;
  mcp = p - 6;
}

void Assembler::compare(IR *ins, int cc) {
  IRRef lref = ins->op1(), rref = ins->op2();
  int32_t imm = 0;

  Reg left = alloc1(lref, kGPR);
  if (is32BitLiteral(rref, &imm)) {
    guardcc(cc);
    emit_gmrmi(XG_ARITHi(XOg_CMP), left | REX_64, imm);
  } else {
    Reg right = alloc1(rref, kGPR);
    guardcc(cc);
    emit_mrm(XO_CMP, left | REX_64, right | REX_64);
  }
}

void Assembler::fieldLoad(IR *ins) {
  IRRef fref = ins->op1();
  LC_ASSERT(fref && ir(fref)->opcode() == IR::kFREF);
  IR *frefins = ir(fref);
  IRRef base = frefins->op1();
  int fieldid = frefins->op2();
  Reg dst = destReg(ins, kGPR);
  Reg basereg = alloc1(base, kGPR);
  load_u64(dst, basereg, sizeof(Word) * fieldid);
}

void Assembler::heapCheck(IR *ins) {
  int32_t bytes = sizeof(Word) * ins->op1();
  if (bytes != 0) {
    guardcc(CC_A);

    // HpLim == [rsp + HPLIM_SP_OFFS]
    emit_rmro(XO_CMP, RID_HP | REX_64, RID_ESP | REX_64, HPLIM_SP_OFFS);

    // TODO: Emit guard
    emit_rmro(XO_LEA, RID_HP | REX_64, RID_HP | REX_64, bytes);
  }
}

void Assembler::insNew(IR *ins) {
  IRBuffer::HeapEntry eid = ins->op2();
  AbstractHeapEntry &entry = buf_->heap_.entry(eid);
  LC_ASSERT(ir(entry.ref()) == ins);
  int ofs = entry.hpOffset();

  // TODO: The result of an allocation is fuseable.  OTOH, we can
  // almost always do store-to-load forwarding, so not sure if that
  // matters.
  Reg dest = destReg(ins, kGPR);
  emit_rmro(XO_LEA, dest | REX_64, RID_HP | REX_64,
            ofs * sizeof(Word));

  // Initialise fields backwards.
  for (int i = entry.size() - 1; i >= 0; --i) {
    IRRef ref = buf_->getField(eid, i);
    memstore(RID_HP, sizeof(Word) * (ofs + 1 + i), ref, kGPR);
  }
  // Write info table.
  memstore(RID_HP, sizeof(Word) * ofs, ins->op1(), kGPR);
}

void Assembler::insUpdate(IR *ins) {
  Reg oldptr = alloc1(ins->op1(), kGPR);
  memstore(oldptr, sizeof(Word), ins->op2(), kGPR);
}

void Assembler::emit(IR *ins) {
  switch (ins->opcode()) {
  case IR::kSLOAD:
    emitSLOAD(ins);
    break;
  case IR::kADD:
    intArith(ins, XOg_ADD);
    break;
  case IR::kSUB:
    intArith(ins, XOg_SUB);
    break;
  case IR::kDIV:
    LC_ASSERT(isIntegerType(ins->type()));
    divmod(ins, DIVMOD_DIV, isSigned(ins->type()));
    break;
  case IR::kREM:
    LC_ASSERT(isIntegerType(ins->type()));
    divmod(ins, DIVMOD_MOD, isSigned(ins->type()));
    break;
  case IR::kSAVE:
    save(ins);
    break;
  case IR::kLT:
  case IR::kGE:
  case IR::kLE:
  case IR::kGT:
  case IR::kEQ:
  case IR::kNE: {
    int idx = (int)ins->opcode() - (int)IR::kLT;
    LC_ASSERT(idx >= 0 && idx < countof(asm_compmap));
    LC_ASSERT(buf_->snap(snapno_).ref() == curins_);
    LC_ASSERT(ir(curins_) == ins);
    compare(ins, asm_compmap[idx] & 15);
    break;
  }
  case IR::kEQINFO:
    itblGuard(ins);
    break;
  case IR::kHEAPCHK:
    heapCheck(ins);
    break;
  case IR::kNEW:
    insNew(ins);
    break;
  case IR::kFREF:
    // Always fused into its use sites.
    break;
  case IR::kFLOAD:
    fieldLoad(ins);
    break;
  case IR::kUPDATE:
    insUpdate(ins);
    break;
  default:
    cerr << "NYI: codegen for ";
    ins->debugPrint(cerr, REF_BIAS + (IRRef1)(ins - ir_));
    cerr << endl;
    exit(23);
  }
}

void Assembler::snapshotAlloc1(IRRef ref) {
  IR *ins = ir(ref);
  if (!ins->hasRegOrSpill()) {
    RegSet allow = kGPR;
    if (!freeset_.intersect(allow).isEmpty()) {
      allocRef(ref, allow);
      RA_DBGX((this, "snapreg   $f $r", ref, ins->reg()));
    } else {
      spill(ins);
      RA_DBGX((this, "snapspill $f $s", ref, ins->spill()));
    }
  }
}

/// Allocate registers to refs escaping to a snapshot.
void Assembler::snapshotAlloc(Snapshot &snap, SnapshotData *snapmap) {
  RA_DBGX((this, "<<SNAP $x>>", snapno_));
  for (Snapshot::MapRef se = snap.begin(); se != snap.end(); ++se) {
    IRRef ref = snapmap->slotRef(se);
    if (!irref_islit(ref)) {
      snapshotAlloc1(ref);
    }
  }
}

void Assembler::save(IR *ins) {
  LC_ASSERT(ins->opcode() == IR::kSAVE);
  SnapNo snapno = snapno_;
  int loop = ins->op1(); // A boolean really.
  Snapshot &snap = buf_->snap(snapno);
  SnapshotData *snapmap = buf_->snapmap();
  int relbase = snap.relbase();

  if (!loop)
    exitTo(snapno);

  // Adjust base pointer if necessary.
  if (relbase != 0) {
    if (relbase > 0) {
      // TODO: Stack check (done after adjusting BASE)
    }
    RA_DBGX((this, "<<base += $x words>>", relbase));
    // TODO: Use LEA?
    emit_gri(XG_ARITHi(XOg_ADD), RID_BASE | REX_64, relbase * sizeof(Word));
  }

  for (Snapshot::MapRef se = snap.begin(); se != snap.end(); ++se) {
    int slot = snapmap->slotId(se);
    IRRef ref = snapmap->slotRef(se);
    RegSet allow = kGPR;

    memstore(RID_BASE, slot * sizeof(Word), ref, allow);
  }
}

void Assembler::emit_jmp(MCode *target) {
  MCode *p = mcp;
  *(int32_t *)(p - 4) = jmprel(p, target);
  p[-5] = XI_JMP;
  mcp = p - 5;
}

void Assembler::exitTo(SnapNo snapno) {
  MCode *target = exitstubAddr(snapno);
  emit_jmp(target);
}

void Assembler::memstore(Reg base, int32_t ofs, IRRef ref, RegSet allow) {
  int32_t k;
  if (irref_islit(ref) && is32BitLiteral(ref, &k)) {
    storei_u64(base, ofs, k);
  } else {
    Reg r = alloc1(ref, allow);
    store_u64(base, ofs, r);
  }
}

#define EXITSTUBS_PER_GROUP 32
#define EXITSTUB_SPACING    (2+2)

MCode *Assembler::exitstubAddr(ExitNo exitno) {
  // Cast to char because we're calculating in bytes.
  char **group = (char **)jit_->exitStubGroup_;
  LC_ASSERT(group[exitno / EXITSTUBS_PER_GROUP] != NULL);
  return (MCode *)(group[exitno / EXITSTUBS_PER_GROUP] +
                   EXITSTUB_SPACING * (exitno % EXITSTUBS_PER_GROUP));
}

MCode *Assembler::generateExitstubGroup(ExitNo group, MachineCode *mcode) {
  ExitNo i;
  ExitNo groupofs = (group * EXITSTUBS_PER_GROUP) & 0xff;
  MCode *mxp = mcbot;
  MCode *mxpstart = mxp;
  if (mxp + (2 + 2) * EXITSTUBS_PER_GROUP + 8 + 5 >= mctop) {
    cerr << "NYI: Overflow when generating exit stubs." << endl;
    exit(2);
  }
  // For each ExitNo in the group generate:
  //     push $(exitno)   // 8-bit immediate
  //     jmp END      // 8-bit offset
  *mxp++ = XI_PUSHi8;
  *mxp++ = (MCode)groupofs;  // groupofs + 0
  for (i = 1; i < EXITSTUBS_PER_GROUP; i++) {
    // Branch for the previous exitno
    *mxp++ = XI_JMPs;
    *mxp++ = (MCode)((2 + 2) * (EXITSTUBS_PER_GROUP - i) - 2);
    // The next exitno.
    *mxp++ = XI_PUSHi8;
    *mxp++ = (MCode)(groupofs + i);
  }
  // We don't need a jump for the final exit in the group.  It's just
  // a fall-through.

  // This is where where the END label occurs.

  // Push the high byte of the exit no.
  *mxp++ = XI_PUSHi8;
  *mxp++ = (MCode)((group * EXITSTUBS_PER_GROUP) >> 8);

  // Jump (relative) to exit handler.
  *mxp++ = XI_JMP;
  mxp += 4;   // The jump is relative to the *end* of the instruction.
  *((int32_t *)(mxp - 4)) = jmprel(mxp, (MCode *)(void *)asmExit);

  // Commit code for this group (even if assembly fails later on).
  mcode->commitStub(mxp);
  mcbot = mxp;
  mclim = mcbot + MCLIM_REDZONE;

  return mxpstart;
}

void Assembler::setupExitStubs(ExitNo nexits, MachineCode *mcode) {
  ExitNo i;
  if (nexits >= EXITSTUBS_PER_GROUP * 16) {
    cerr << "FATAL: Too many exit stubs! (" << nexits << ")" << endl;
    exit(3);
  }
  ExitNo ngroups = (nexits + EXITSTUBS_PER_GROUP + 1) / EXITSTUBS_PER_GROUP;
  for (i = 0; i < ngroups; ++i) {
    if (jit_->exitStubGroup_[i] == NULL) {
      jit_->exitStubGroup_[i] = generateExitstubGroup(i, mcode);
      if (true) {
        MCode *start = jit_->exitStubGroup_[i];
        MCode *end = mcode->stubEnd();
        ios_base::openmode mode = (i == 0) ? ios_base::trunc : ios_base::app;
        ofstream out;
        out.open("dump_exitstubs.s", ios_base::out | mode);
        out << ".text\n" "stub_group_" << (int)i << ":\n";
        mcode->dumpAsm(out, start, end);
        out.close();
      }
    }
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
