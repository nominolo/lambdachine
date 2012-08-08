#include "jit.hh"
#include "ir-inl.hh"
#include "assembler.hh"

#include <iostream>
#include <string.h>


_START_LAMBDACHINE_NAMESPACE

using namespace std;

HotCounters::HotCounters(HotCount threshold)
  : threshold_(threshold) {
  for (Word i = 0; i < kNumCounters; ++i) {
    counters_[i] = threshold;
  }
}

Jit::Jit()
  : cap_(NULL),
    startPc_(NULL), startBase_(NULL),
    flags_(), targets_(), fragments_(),
    prng_(), mcode_(&prng_), asm_(this) {
  memset(exitStubGroup_, 0, sizeof(exitStubGroup_));
}

Jit::~Jit() {
  for (FRAGMENT_MAP::iterator it = fragments_.begin();
       it != fragments_.end(); ++it) {
    delete it->second;
  }
}

void Jit::beginRecording(Capability *cap, BcIns *startPc, Word *base, bool isReturn) {
  LC_ASSERT(cap_ == NULL);
  LC_ASSERT(targets_.size() == 0);
  cap_ = cap;
  startPc_ = startPc;
  startBase_ = base;
  flags_.clear(kLastInsWasBranch);
  flags_.set(kIsReturnTrace, isReturn);
}

bool Jit::recordIns(BcIns *ins, Word *base) {
  cerr << "REC: " << ins << " " << ins->name() << endl;
  if (flags_.get(kLastInsWasBranch)) {
    if (ins == startPc_) {
      cerr << "REC: Loop detected." << endl
           << "  Loop: " << startPc_ << endl;
      for (size_t i = 0; i < targets_.size(); ++i) {
        cerr << "    " << targets_[i] << endl;
      }
      finishRecording();
      return true;
    } else if (targets_.size() <= 100) {
      // Try to find inner loop.
      for (size_t i = 0; i < targets_.size(); ++i) {
        if (targets_[i] == ins) {
          cerr << COL_GREEN << "REC: Inner loop." << COL_RESET << endl;

          // TODO: Truncate.

          resetRecorderState();
          return true;
        }
      }

      targets_.push_back(ins);
    } else {
      cerr << COL_RED << "TRACE TOO LONG (" << targets_.size()
           << ")" << COL_RESET << endl;
      cerr << "    " << startPc_ << endl;
      for (size_t i = 0; i < targets_.size(); ++i) {
        if ((i % 7) == 0) {
          cerr << "   ";
        }
        cerr << ' ' << i << ':' << targets_[i];
        if (((i + 1) % 7) == 0) cerr << endl;
      }
      cerr << endl;

      resetRecorderState();
      return true;
    }
  }

  switch (ins->opcode()) {
  case BcIns::kCALL:
  case BcIns::kCALLT:
  case BcIns::kEVAL:
  case BcIns::kRET1:
    flags_.set(kLastInsWasBranch);
    break;
  default:
    flags_.clear(kLastInsWasBranch);
    break;
  }
  return false;
}

inline void Jit::resetRecorderState() {
  flags_.clear();
  targets_.clear();
  cap_ = NULL;
}

void Jit::finishRecording() {
  Fragment *F = new Fragment();
  F->traceId_ = fragments_.size();
  F->numTargets_ = targets_.size();
  F->targets_ = new BcIns*[F->numTargets_];
  F->startPc_ = startPc_;
  for (uint32_t i = 0; i < F->numTargets_; ++i) {
    F->targets_[i] = targets_[i];
  }
  registerFragment(startPc_, F);
  resetRecorderState();
}

Fragment::Fragment()
  : flags_(0), traceId_(0), startPc_(NULL), targets_(NULL) {
}

Fragment::~Fragment() {
  if (targets_ != NULL)
    delete[] targets_;
}

void Jit::genCode(IRBuffer *buf) {
  IRRef ref;
  for (ref = buf->bufmax_; ref > REF_BASE; --ref) {
    IR *tir = buf->ir(ref);
    genCode(buf, tir);
  }
}

Fragment *Jit::saveFragment() {
  IRBuffer *buf = &buf_;
  Assembler *as = &asm_;

  Fragment *F = new Fragment();
  F->startPc_ = startPc_;

  F->numTargets_ = targets_.size();
  F->targets_ = new BcIns*[F->numTargets_];
  for (size_t i = 0; i < F->numTargets_; ++i)
    F->targets_[i] = targets_.at(i);

  long bufsize = (long)buf->bufmax_ - (long)buf->bufmin_;
  IR *buffer = new IR[bufsize];
  F->firstconstant_ = buf->bufmin_;
  F->nextins_ = buf->bufmax_;
  buffer = biasBuffer(buffer, -(F->firstconstant_ - REF_BIAS));
  for (IRRef ref = F->firstconstant_; ref < F->nextins_; ++ref)
    buffer[ref] = buf->buffer_[ref]; // TODO: use memcpy
  F->buffer_ = buffer;

  size_t nsnaps = buf->snaps_.size();
  F->nsnaps_ = nsnaps;
  F->snaps_ = new Snapshot[nsnaps];
  for (size_t i = 0; i < nsnaps; ++i)
    F->snaps_[i] = buf->snaps_.at(i);
  F->snapmap_.data_ = buf->snapmap_.data_;
  F->snapmap_.index_ = buf->snapmap_.data_.size();

  F->mcode_ = as->mcp;

  return F;
}

#if (DEBUG_COMPONENTS & DEBUG_TRACE_ENTEREXIT) != 0
#define DBG(stmt) do { stmt; } while(0)
#else
#define DBG(stmt) do {} while(0)
#endif

inline uint64_t Fragment::literalValue(IRRef ref, Word *base) {
  IR *ins = ir(ref);
  if (ins->opcode() == IR::kKINT) {
    if (kOpIsSigned & (1 << (int)ins->type()))
      return (int64_t)(int32_t)ins->i32();
    else
      return (uint64_t)(uint32_t)ins->i32();
  } else if (ins->opcode() == IR::kKWORD) {
    return (uint64_t)ins->u32() | ((uint64_t)ir(ref - 1)->u32() << 32);
  } else if (ins->opcode() == IR::kKBASEO) {
    return (uint64_t)(base + ins->i32());
  }
  return 0;
}

void Fragment::restoreSnapshot(ExitNo exitno, ExitState *ex) {
  Word *spill = ex->spill;
  LC_ASSERT(0 <= exitno && exitno < nsnaps_);
  DBG(cerr << "Restoring from snapshot " << (int)exitno << endl);
  Snapshot &sn = snap(exitno);
  IR *snapins = ir(sn.ref());
  if (snapins->opcode() != IR::kSAVE) {
    Word *base = (Word *)ex->gpr[RID_BASE];
    void *hp = (Word *)ex->gpr[RID_HP];
    DBG(sn.debugPrint(cerr, &snapmap_, exitno));
    DBG(cerr << "  base = " << base << ", hp = " << hp << endl);
    for (Snapshot::MapRef i = sn.begin(); i < sn.end(); ++i) {
      int slot = snapmap_.slotId(i);
      int ref = snapmap_.slotRef(i);
      IR *ins = ir(ref);
      DBG(cerr << "    Restoring "; IR::printIRRef(cerr, ref));
      DBG(cerr << ":  base[" << slot << "] = ");
      if (irref_islit(ref)) {
        uint64_t k = literalValue(ref, base);
        DBG(cerr << "literal (" << hex << k << ")" << endl);
        base[slot] = k;
      } else if (ins->spill() != 0) {
        DBG(cerr << "spill[" << (int)ins->spill() << "] ("
            << hex << spill[ins->spill()] << ")" << endl);
        base[slot] = spill[ins->spill()];
      } else {
        LC_ASSERT(isReg(ins->reg()));
        DBG(cerr << IR::regName(ins->reg(), ins->type()) << " ("
            << hex << ex->gpr[ins->reg()] << ")" << endl);
        base[slot] = ex->gpr[ins->reg()];
      }
    }
  }
}

#undef DBG

#define SLOT_SIZE (LC_ARCH_BITS/8)

static void asmSLOAD(Assembler *as, IR *ir) {
  int32_t ofs = SLOT_SIZE * (int16_t)ir->op1();
  Reg base = RID_BASE;
  RegSet allow = kGPR;
  Reg dst = as->destReg(ir, allow);
  as->load_u64(dst, base, ofs);
}

void Jit::genCode(IRBuffer *buf, IR *ir) {
  switch (ir->opcode()) {
  case IR::kSLOAD:
    asmSLOAD(&asm_, ir);
    break;
  default:
    exit(11);
    //    load_u64(
  }
}

_END_LAMBDACHINE_NAMESPACE
