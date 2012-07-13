#include "ir.hh"
#include <iostream>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

uint8_t IR::mode[k_MAX + 1] = {
#define IRMODE(name, flags, left, right) IRM_##flags,
  IRDEF(IRMODE)
#undef IRMODE
  0
};

IRBuffer::IRBuffer()
  : size_(1024) {
  realbuffer_ = new IR[size_];

  size_t nliterals = size_ / 4;
  
  bufstart_ = REF_BIAS - nliterals;
  bufend_ = bufstart_ + size_;

  // We want to have:
  //
  //     buffer_[REF_BIAS] = realbuffer_[nliterals];
  //
  // Thus:
  //
  //     buffer_ + REF_BIAS = realbuffer_ + nliterals
  //
  buffer_ = realbuffer_ + (nliterals - REF_BIAS);
  bufmin_ = REF_BIAS - 1;
  bufmax_ = REF_BIAS;

  emitRaw(IRT(IR::kBASE, IRT_PTR), 0, 0);
}

IRBuffer::~IRBuffer() {
  delete[] realbuffer_;
  realbuffer_ = NULL;
  buffer_ = NULL;
}

void IRBuffer::growTop() {
  cerr << "NYI: Growing IR buffer\n";
  exit(3);
}

TRef IRBuffer::emit() {
  IRRef ref = nextIns();
  IR *ir1 = ir(ref);
  IR::Opcode op = fold_.ins.opcode();

  ir1->setPrev(chain_[op]);
  chain_[op] = (IRRef1)ref;
  
  ir1->setOpcode(op);
  ir1->setOp1(fold_.ins.op1());
  ir1->setOp2(fold_.ins.op2());
  IR::Type t = fold_.ins.t();
  ir1->setT(t);
  
  return TRef(ref, t);
}

TRef IRBuffer::optFold() {
  // TODO: Currently no optimisations are performed
  return emit();
}

TRef IRBuffer::optCSE() {
  IRRef2 op12 = 
    (IRRef2)fins()->op1() + ((IRRef2)fins()->op2() << 16);
  IR::Opcode op = fins()->opcode();
  if (true /* TODO: check if CSE is enabled */) {
    IRRef ref = chain_[op];
    IRRef lim = fins()->op1();
    if (fins()->op2() > lim) lim = fins()->op2();

    while (ref > lim) {
      if (ir(ref)->op12() == op12) {
        // Common subexpression found
        return TRef(ref, ir(ref)->t());
      }
      ref = ir(ref)->prev();
    }
  }
  // Otherwise emit IR
  return emit();
}

_END_LAMBDACHINE_NAMESPACE
