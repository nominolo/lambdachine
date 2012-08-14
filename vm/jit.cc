#include "jit.hh"
#include "ir-inl.hh"
#include "assembler.hh"
#include "thread.hh"
#include "capability.hh"

#include <iostream>
#include <string.h>
#include <fstream>
#include <sstream>
#include <iomanip>


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
  buf_.reset(base, cap->currentThread()->top());
  lastResult_ = TRef();
}

static IRType littypeToIRType(uint8_t littype) {
  // TODO: Check that this gets compiled to range check + lookup
  // table.
  switch (littype) {
  case LIT_INT:
    return IRT_I64;
  case LIT_CHAR:
    return IRT_CHR;
  case LIT_STRING:
    return IRT_PTR;
  case LIT_WORD:
    return IRT_U64;
  case LIT_CLOSURE:
    return IRT_CLOS;
  case LIT_INFO:
    return IRT_INFO;
  case LIT_PC:
    return IRT_PC;
  default:
    return IRT_UNKNOWN;
  }
}

bool Jit::recordIns(BcIns *ins, Word *base, const Code *code) {
  buf_.pc_ = ins;
  cerr << "REC: " << ins << " " << ins->name() << endl;
  if (flags_.get(kLastInsWasBranch)) {
    if (ins == startPc_) {
      cerr << "REC: Loop detected." << endl
           << "  Loop: " << startPc_ << endl;
      for (size_t i = 0; i < targets_.size(); ++i) {
        cerr << "    " << targets_[i] << endl;
      }
      buf_.emit(IR::kSAVE, IRT_VOID | IRT_GUARD, IR_SAVE_LOOP, 0);
      finishRecording();
      return true;
    } else if (targets_.size() <= 100) {
      // Try to find inner loop.
      for (size_t i = 0; i < targets_.size(); ++i) {
        if (targets_[i] == ins) {
          cerr << COL_GREEN << "REC: Inner loop." << COL_RESET << endl;

          // TODO: Truncate.
          goto abort_recording;
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

      goto abort_recording;
    }
  }

  switch (ins->opcode()) {
  case BcIns::kFUNC:
    break;
  case BcIns::kLOADK: {
    u2 lit_id = ins->d();
    Word lit = code->lits[lit_id];
    IRType ty = littypeToIRType(code->littypes[lit_id]);
    TRef litref = buf_.literal(ty, lit);
    buf_.setSlot(ins->a(), litref);
    break;
  }
  case BcIns::kISGT: {
    bool taken = (WordInt)base[ins->a()] > (WordInt)base[ins->d()];
    TRef aref = buf_.slot(ins->a());
    TRef bref = buf_.slot(ins->d());
    buf_.emit(taken ? IR::kGT : IR::kLE, IRT_VOID | IRT_GUARD, aref, bref);
    break;
  }
  case BcIns::kSUBRR: {
    TRef bref = buf_.slot(ins->b());
    TRef cref = buf_.slot(ins->c());
    TRef aref = buf_.emit(IR::kSUB, IRT_I64, bref, cref);
    buf_.setSlot(ins->a(), aref);
    break;
  }
  case BcIns::kADDRR: {
    TRef bref = buf_.slot(ins->b());
    TRef cref = buf_.slot(ins->c());
    TRef aref = buf_.emit(IR::kADD, IRT_I64, bref, cref);
    buf_.setSlot(ins->a(), aref);
    break;
  }

  case BcIns::kCALLT: {
    // TODO: Detect and optimise recursive calls into trace specially?
    TRef fnode = buf_.slot(ins->a());
    Closure *clos = (Closure *)base[ins->a()];
    InfoTable *info = clos->info();
    TRef iref = buf_.literal(IRT_INFO, (Word)info);
    buf_.setSlot(-1, fnode);  // Write to slot before the guard.
    // Clear all non-argument registers.
    for (int i = ins->c(); i < code->framesize; ++i) {
      buf_.setSlot(i, TRef());
    }
    buf_.emit(IR::kEQINFO, IRT_VOID | IRT_GUARD, fnode, iref);

    if (ins->c() != ((FuncInfoTable *)info)->code()->arity) {
      FuncInfoTable *itbl = ((FuncInfoTable *)info);
      cerr << "NYI: Recording of non-exact applications" << endl;
      cerr << "  args=" << (int)ins->c() << "  arity="
           << (int)itbl->code()->arity << "  name="
           << itbl << endl;
      exit(4);
      goto abort_recording;
    }
    flags_.set(kLastInsWasBranch);
    break;
  }
  case BcIns::kMOV:
    buf_.setSlot(ins->a(), buf_.slot(ins->d()));
    break;

  case BcIns::kEVAL: {
    Closure *tnode = (Closure *)base[ins->a()];
    if (tnode->isIndirection()) {
      cerr << "NYI: EVAL of indirections" << endl;
      goto abort_recording;
    }
    if (!tnode->isHNF()) {
      cerr << "NYI: EVAL of thunk" << endl;
    }
    TRef noderef = buf_.slot(ins->a());
    TRef inforef = buf_.literal(IRT_INFO, (Word)tnode->info());
    buf_.emit(IR::kEQINFO, IRT_VOID | IRT_GUARD, noderef, inforef);
    lastResult_ = noderef;
    // TODO: Clear dead registers.
    break;
  }

  case BcIns::kMOV_RES: {
    if (!(IRRef)lastResult_) {
      cerr << "NYI: MOV_RES with out-of trace input." << endl;
      goto abort_recording;
    }
    buf_.setSlot(ins->a(), lastResult_);
    break;
  }

  case BcIns::kLOADF: {
    TRef rbase = buf_.slot(ins->b());
    TRef fref = buf_.emit(IR::kFREF, IRT_PTR, rbase, ins->c());
    TRef res = buf_.emit(IR::kFLOAD, IRT_UNKNOWN, fref, 0);
    buf_.setSlot(ins->a(), res);
    break;
  }

  default:
    cerr << "NYI: Recording of " << ins->name() << endl;
    goto abort_recording;

    // case BcIns::kCALL:
    // case BcIns::kEVAL:
    // case BcIns::kRET1:
    //   flags_.set(kLastInsWasBranch);
    //   break;
    // default:
    //   flags_.clear(kLastInsWasBranch);
    //   break;
  }
  return false;

abort_recording:
  resetRecorderState();
  return true;
}

inline void Jit::resetRecorderState() {
  flags_.clear();
  targets_.clear();
  cap_ = NULL;
}

void Jit::finishRecording() {
  cerr << "Recorded: " << endl;
  asm_.setup(buffer());
  asm_.assemble(buffer(), mcode());
  buf_.debugPrint(cerr, 1);

  int tno = fragments_.size();

  ofstream out;
  stringstream filename;
  filename << "dump_Trace_" << (int)tno << ".s";
  out.open(filename.str().c_str());
  mcode()->dumpAsm(out);
  out.close();

  //  exit(2);

  Fragment *F = saveFragment();

  registerFragment(startPc_, F);
  *startPc_ = BcIns::ad(BcIns::kJFUNC, 0, tno);
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
  F->traceId_ = fragments_.size();
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

  LC_ASSERT(buf->slots_.highestSlot() >= 0);
  F->spillOffset_ = buf->slots_.highestSlot();
  F->frameSize_ = buf->slots_.highestSlot() + as->spill_;

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

static void printRegisters(ostream &out, Word *gpr) {
  for (RegSet work = kGPR; !work.isEmpty(); ) {
    Reg r = work.pickBot();
    work.clear(r);
    cerr << "    " << setw(3) << left << IR::regName(r, IRT_I64)
         << " = 0x"
         << setw(16) << setfill('0') << right << hex << gpr[r]
         << " / "
         << setw(20) << setfill(' ') << right << dec << (WordInt)gpr[r]
         << endl;
  }
}

static void printExitState(ostream &out, ExitState *ex) {
  Word *base = (Word*)ex->gpr[RID_BASE];
  Word *hp = (Word*)ex->gpr[RID_HP];
  out << "  base = " << base << ", hp = " << hp
      << ", hplim = " << ex->hplim
      << ", spill=" << ex->spill
      << " (delta=" << hex << (char *)ex->spill - (char *)base
      << endl;
  printRegisters(cerr, &ex->gpr[0]);
}

Word *traceDebugLastHp = NULL;

void Fragment::restoreSnapshot(ExitNo exitno, ExitState *ex) {
  Word *spill = ex->spill;
  LC_ASSERT(0 <= exitno && exitno < nsnaps_);
  DBG(cerr << "Restoring from snapshot " << (int)exitno << endl);
  Snapshot &sn = snap(exitno);
  IR *snapins = ir(sn.ref());
  Word *base = (Word *)ex->gpr[RID_BASE];
  traceDebugLastHp = NULL;
  if (snapins->opcode() != IR::kSAVE) {
    DBG(sn.debugPrint(cerr, &snapmap_, exitno));
    DBG(printExitState(cerr, ex));
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
            << hex << spill[ins->spill()] << "/"
            << dec << spill[ins->spill()] << ")"
            << endl);
        base[slot] = spill[ins->spill()];
      } else {
        LC_ASSERT(isReg(ins->reg()));
        DBG(cerr << IR::regName(ins->reg(), ins->type()) << " ("
            << hex << ex->gpr[ins->reg()] << ")" << endl);
        base[slot] = ex->gpr[ins->reg()];
      }
    }
  }
  if (sn.relbase() != 0) {
    cerr << "NYI: non-zero relbase" << endl;
    exit(3);
  }
  ex->T->base_ = base;
  ex->T->pc_ = sn.pc();

  Capability *cap = ex->T->owner();
  LC_ASSERT(cap != NULL);
  cap->traceExitHp_ = (Word *)ex->gpr[RID_HP];
  cap->traceExitHpLim_ = ex->hplim;
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

extern "C" void LC_USED
debugTrace(ExitState *ex) {
  cerr << "Debug trace called" << endl;
  printExitState(cerr, ex);
  if (traceDebugLastHp != NULL) {
    cerr << "Allocated: " << endl;
    Word *hp = traceDebugLastHp;
    Word *newhp = (Word*)ex->gpr[RID_HP];
    int n = 0;
    while (hp < newhp) {
      if ((n % 4) == 0)
        cerr << hp << ":";
      cerr << " " << setw(16) << setfill('0') << right << hex << *hp;
      if ((++n % 4) == 0) cerr << endl;
      ++hp;
    }
    cerr << endl;
  }
  traceDebugLastHp = (Word*)ex->gpr[RID_HP];
}

_END_LAMBDACHINE_NAMESPACE
