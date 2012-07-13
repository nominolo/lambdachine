#include "ir.hh"

#include <iostream>
#include <iomanip>

#include <string.h>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

uint8_t IR::mode_[k_MAX + 1] = {
#define IRMODE(name, flags, left, right) \
  (((IRM##left) | ((IRM##right) << 2)) | IRM_##flags),
  IRDEF(IRMODE)
#undef IRMODE
  0
};

#define STR(x) #x

const char *IR::name_[k_MAX + 1] = {
#define IRNAME(name, flags, left, right) STR(name),
  IRDEF(IRNAME)
#undef IRNAME
  "????"
};

static const char tyname[][IRT_TYPE] = {
  "unk", "   ", "i32", "u32", "chr", "f32", "f64",
  "cls", "inf", "pc ", "ptr"
};

enum {
  TC_NONE, TC_PRIM, TC_HEAP, TC_GREY,
  TC_MAX
};

static const uint8_t tycolor[IRT_TYPE] = {
  TC_GREY, TC_NONE, TC_PRIM, TC_PRIM, TC_PRIM, TC_PRIM, TC_PRIM,
  TC_HEAP, TC_NONE, TC_NONE, TC_NONE,
};

static const char *tycolorcode[TC_MAX] = {
  "", COL_BLUE, COL_RED, COL_GREY
};

void IR::printIRRef(std::ostream &out, IRRef ref) {
  out << setw(4) << dec << setfill('0') << (ref - REF_BIAS);
}

static void printArg(ostream &out, uint8_t mode, uint16_t op, IR *ir) {
  switch ((IR::Mode)mode) {
  case IR::IRMnone:
    break;
  case IR::IRMref:
    out << ' ';
    IR::printIRRef(out, (IRRef)op);
    break;
  case IR::IRMlit:
    out << " #";
    out << setw(3) << setfill(' ') << left << (unsigned int)op;
    break;
  case IR::IRMcst:
    out << "<cst>";
    break;
  default:
    break;
  }
}

void IR::debugPrint(ostream &out, IRRef self) {
  IR::Opcode op = opcode();
  uint8_t ty = t() & IRT_TYPE;
  IR::printIRRef(out, self);
  out << "    "; // TODO: flags go here
  out << tycolorcode[tycolor[ty]];
  out << tyname[ty] << COL_RESET << ' ';
  out << setw(8) << setfill(' ') << left << name_[op];
  uint8_t mod = mode(op);
  printArg(out, mod & 3, op1(), this);
  printArg(out, (mod >> 2) & 3, op2(), this);
  out << endl;
}

IRBuffer::IRBuffer(Word *base, Word *top)
  : size_(1024), slots_(base, top) {
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
  IR::Opcode op = fins()->opcode();
  IR::IRMode irmode = IR::mode(op);
  if (op == IR::kSLOAD) {

  } else if ((irmode & IR::IRM_S) == IR::IRM_N) {
    // If it's not a store/load/alloc, do CSE.
    return optCSE();
  }
  // TODO: Currently no other optimisations are performed
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

AbstractStack::AbstractStack(Word *base, Word *top) {
  slots_ = new TRef[kSlots];
  base_ = kInitialBase;
  LC_ASSERT(base < top);
  realOrigBase_ = base;
  top_ = base_ + (top - base);
  LC_ASSERT(top_ < kSlots);
  low_ = base_;
  high_ = top_;
}

bool AbstractStack::frame(Word *base, Word *top) {
  int delta = base - realOrigBase_;
  if (delta < -(kInitialBase - 1)) return false;  // underflow
  base_ = kInitialBase + delta;
  top_ = base_ + (top - base);
  if (top_ >= kSlots) return false; // overflow
  return true;
}

_END_LAMBDACHINE_NAMESPACE
