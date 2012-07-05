#include "jit.hh"

#include <iostream>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

HotCounters::HotCounters(HotCount threshold)
  : threshold_(threshold) {
  for (int i = 0; i < kNumCounters; ++i) {
    counters_[i] = threshold;
  }
}

Jit::Jit()
  : mcode_(&prng_)
{
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
      for (int i = 0; i < targets_.size(); ++i) {
        cerr << "    " << targets_[i] << endl;
      }
      finishRecording();
      return true;
    } else if (targets_.size() <= 100) {
      // Try to find inner loop.
      for (int i = 0; i < targets_.size(); ++i) {
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
      for (int i = 0; i < targets_.size(); ++i) {
        if ((i % 7) == 0) { cerr << "   "; }
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
  for (int i = 0; i < F->numTargets_; ++i) {
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

_END_LAMBDACHINE_NAMESPACE
