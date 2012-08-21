#include "ir.hh"
#include "objects.hh"

#include <iostream>

_START_LAMBDACHINE_NAMESPACE

using namespace std;

/// Folding / Simplification

/// The fold engine performs basic algebraic optimisations including
/// constant folding.  It is implemented as a collection of folding
/// rules that work on the FoldState.
///
/// When the IRBuffer wants to emit an instruction to the list, it
/// puts the new instruction into fold_.ins.  The fold engine then
/// tries to perform optimisations and possibly rewrites fold_.ins.
///
/// The outcome of each fold rule is one of the following:
///
///   - NEXTFOLD: The fold rule didn't apply and the next rule should
///     be tried (if any).
///
///   - RETRYFOLD: The fold rule did apply and transformed fold_.ins.
///     This may give rise to new optimisations so the fold engine
///     should retry all optimisations on the new instruction.
///
///   - KLITFOLD: The folding engine was able to completely optimise
///     the new instruction to a literal.
///
///   - FAILFOLD: The instruction is a guard that will always fail.
///
///   - DROPFOLD: The instruction is a guard that will always succeed.
///
///   - An IR reference: The result of the instruction is equivalent
///     to the value of an instruction already in the instruction
///     stream.
///
/// This means that fold rules must be tried starting from most
/// specific to most general.
///
/// The fold engine only modifies the FoldState and does not modify
/// the IRBuffer, except for possibly emitting literals.  Due to
/// optimisations some instructions may no longer be needed.  These
/// are later removed during dead code elmination (DCE).
///
/// When optimising unrolled loops it is important that fold rules do
/// not simplify loop-variant (i.e., PHI-bound) variables.  To protect
/// against this, use PHIBARRIER.

enum {
  NEXTFOLD,  // Couldn't fold
  RETRYFOLD, // Run the fold engine again
  KLITFOLD,
  FAILFOLD,
  DROPFOLD,
  MAX_FOLD
};

// Prevents an optimisation from crossing the loop boundary.
#define PHIBARRIER(ir) if ((ir).t() & IRT_ISPHI) return NEXTFOLD

// Return the left operand of the current instruction.
#define LEFTFOLD (fold_.ins.op1())

// Return the literal
#define LITFOLD(val) ((fold_.literal = (val)), KLITFOLD)

#define fins   (&fold_.ins)
#define fleft  (&fold_.left)
#define fright (&fold_.right)

/// Defines a single named fold rule.
#define FOLDF(name) static IRRef name(FoldState &fold_, IRBuffer *buf)

/// Pattern matches on the shape of fold_.ins.  Examples:
///
///   - PATTERN(lit, any) matches iff the op1 is a literal.
///
///   - PATTERN(ADD, lit) matches iff the opcode of op1 argument's
///     instruction is IR::kADD and op2 is a literal.
///
#define PATTERN(pred1, pred2, handler) \
  if ((pred1(left, fold_.left.opcode())) && \
      (pred2(right, fold_.right.opcode()))) { \
    ref = handler(fold_, this); if (ref != NEXTFOLD) break; }

#define any(ref,opc) true
#define lit(ref,opc) irref_islit(ref)
#include "irfoldmacros.hh"


static int64_t kfold_intop(int64_t k1, int64_t k2, IR::Opcode op) {
  switch (op) {
  case IR::kADD:
    k1 += k2;
    break;
  case IR::kSUB:
    k1 -= k2;
    break;
  case IR::kMUL:
    k1 *= k2;
    break;
  case IR::kNEG:
    k1 = -k1;
    break;
  default:
    LC_ASSERT(0);
    break;
  }
  return k1;
}

// Constant folding. Both arguments are constants.
FOLDF(kfold_arith) {
  if (fold_.ins.type() == IRT_I64) {
    return LITFOLD(kfold_intop(buf->literalValue(fold_.ins.op1()),
                               buf->literalValue(fold_.ins.op2()),
                               fold_.ins.opcode()));
  }
  return NEXTFOLD;
}

FOLDF(simplify_intadd_k) {
  if (fold_.ins.type() == IRT_I64 &&
      buf->literalValue(fold_.ins.op2()) == 0)
    return LEFTFOLD;
  return NEXTFOLD;
}

// Swap commutative arguments if likely to be benificial.
//
// This moves the smaller reference to the right, which tends to put
// the constant on the RHS.
FOLDF(comm_swap) {
  if (fold_.ins.op1() < fold_.ins.op2()) {
    IRRef1 tmp = fold_.ins.op1();
    fold_.ins.setOp1(fold_.ins.op2());
    fold_.ins.setOp2(tmp);
    return RETRYFOLD;
  }
  return NEXTFOLD;
}

// (x op k1) op k2  =>  x op (k1 op k2)
// (x op k1) op k2  =>  x op k1, if (k1 op k2) == k1
FOLDF(reassoc_int_arith) {
  if (fold_.ins.type() == IRT_I64 &&
      irref_islit(fleft->op2())) {
    int64_t k1 = buf->literalValue(fold_.left.op2());
    int64_t k2 = buf->literalValue(fold_.ins.op2());
    int64_t k = kfold_intop(k1, k2, fold_.ins.opcode());
    if (k == k1) return LEFTFOLD;
    PHIBARRIER(fold_.left);
    fold_.ins.setOp1(fold_.left.op1());
    TRef tr = buf->literal(fold_.ins.type(), k);
    fold_.ins.setOp2(tr.ref());
    return RETRYFOLD;
  }
  return NEXTFOLD;
}

/// i - 0 ==> i
/// i - k ==> i + (-k)
FOLDF(simplify_sub_k) {
  if (isIntegerType(fold_.ins.type())) {
    uint64_t k = buf->literalValue(fold_.ins.op2());
    if (k == 0)
      return LEFTFOLD;
    uint64_t k2 = -(int64_t)k;  // Overflow OK.
    IRRef ref = (IRRef1)buf->literal(fold_.ins.type(), k2);
    fold_.ins.setOpcode(IR::kADD);
    fold_.ins.setOp2(ref);
    return RETRYFOLD;
  } // TODO: i - (+-0) ==> i  for floats
  return NEXTFOLD;
}

/// 0 - i ==> -i
FOLDF(simplify_intsub_kleft) {
  if (isIntegerType(fold_.ins.type())) {
    uint64_t k = buf->literalValue(fold_.ins.op1());
    if (k == 0) {
      fold_.ins.setOpcode(IR::kNEG);
      fold_.ins.setOp1(fold_.ins.op2());
      return RETRYFOLD;
    }
  }
  return NEXTFOLD;
}

/// i - i ==> 0
FOLDF(simplify_intsub) {
  if (fold_.ins.op1() == fold_.ins.op2() &&
      !isFloatType(fold_.ins.type())) {
    return LITFOLD(0);
  }
  return NEXTFOLD;
}

/// (i + j) - i ==> j
/// (i + j) - j ==> i
FOLDF(simplify_intsubadd_leftcancel) {
  if (!isFloatType(fold_.ins.type())) {
    if (fold_.ins.op2() == fold_.left.op1())
      return fold_.left.op2();
    if (fold_.ins.op2() == fold_.left.op2())
      return fold_.left.op1();
  }
  return NEXTFOLD;
}

/// (i - j) - i ==> 0 - j
FOLDF(simplify_intsubsub_leftcancel) {
  if (!isFloatType(fold_.ins.type())) {
    PHIBARRIER(fold_.left);
    if (fold_.left.op1() == fold_.ins.op2()) {
      fold_.ins.setOp1((IRRef1)buf->literal(fold_.ins.type(), 0));
      fold_.ins.setOp2(fold_.left.op2());
      return RETRYFOLD;
    }
  }
  return NEXTFOLD;
}

/// i - (i - j) ==> j
FOLDF(simplify_intsubsub_rightcancel) {
  if (!isFloatType(fold_.ins.type())) {
    PHIBARRIER(fold_.left);
    if (fold_.ins.op1() == fold_.right.op1())
      return fold_.right.op2();
  }
  return NEXTFOLD;
}

/// i - (i + j) ==> 0 - j
/// i - (j + i) ==> 0 - j
FOLDF(simplify_intsubadd_rightcancel) {
  if (!isFloatType(fold_.ins.type())) {
    PHIBARRIER(fold_.right);
    if (fold_.ins.op1() == fold_.right.op1()) {
      fold_.ins.setOp2(fold_.right.op2());
      fold_.ins.setOp1((IRRef1)buf->literal(fold_.ins.type(), 0));
      return RETRYFOLD;
    }
    if (fold_.ins.op1() == fold_.right.op2()) {
      fold_.ins.setOp2(fold_.right.op1());
      fold_.ins.setOp1((IRRef1)buf->literal(fold_.ins.type(), 0));
      return RETRYFOLD;
    }
  }
  return NEXTFOLD;
}

/// (x + y) - (x + z) ==> y - z
/// (x + y) - (z + x) ==> y - z
/// (y + x) - (x + z) ==> y - z
/// (y + x) - (z + x) ==> y - z
FOLDF(simplify_intsubaddadd_cancel) {
  if (!isFloatType(fins->type())) {
    PHIBARRIER(fold_.left);
    PHIBARRIER(fold_.right);
    if (fleft->op1() == fright->op1()) {
      fins->setOp1(fleft->op2());
      fins->setOp2(fright->op2());
      return RETRYFOLD;
    }
    if (fleft->op1() == fright->op2()) {
      fins->setOp1(fleft->op2());
      fins->setOp2(fright->op1());
      return RETRYFOLD;
    }
    if (fleft->op2() == fright->op1()) {
      fins->setOp1(fleft->op1());
      fins->setOp2(fright->op2());
      return RETRYFOLD;
    }
    if (fleft->op2() == fright->op2()) {
      fins->setOp1(fleft->op1());
      fins->setOp2(fright->op1());
      return RETRYFOLD;
    }
  }
  return NEXTFOLD;
}

IRRef IRBuffer::foldHeapcheck() {
  IRRef hpchkref = chain_[IR::kHEAPCHK];
  if (hpchkref /* && hpchkref >= loop_ */) {
    IR *hpchk = ir(hpchkref);
    uint16_t nwords = hpchk->op1() + fins->op1();
    hpchk->setOp1(nwords);
    return DROPFOLD;
  }
  return NEXTFOLD;
}

// Constant-fold an EQGUARD where the closure is a literal. The
// second operand will always be a literal.
FOLDF(kfold_eqinfo) {
  Closure *cl = (Closure *)buf->literalValue(fins->op1());
  InfoTable *itbl = (InfoTable *)buf->literalValue(fins->op2());
  if (cl->info() == itbl)
    return DROPFOLD;
  else
    return FAILFOLD;
}

// Constant fold any arithmetic comparison operation.
FOLDF(kfold_cmp) {
  uint64_t k1 = buf->literalValue(fins->op1());
  uint64_t k2 = buf->literalValue(fins->op2());
  switch (fins->opcode()) {
  case IR::kEQ:
    return (k1 == k2) ? DROPFOLD : FAILFOLD;
  case IR::kNE:
    return (k1 == k2) ? DROPFOLD : FAILFOLD;
  default:
    cerr << "FATAL: kfold_cmp called on unsupported instruction.\n";
    fins->debugPrint(cerr, 0);
    cerr << endl;
    exit(3);
  }
}

// FLOAD (FREF (NEW k [x1 .. xN]) i) ==> x_i
FOLDF(load_fwd) {
  LC_ASSERT(fleft->opcode() == IR::kFREF);
  IRBuffer::HeapEntry entry = buf->getHeapEntry(fleft->op1());
  if (entry != IRBuffer::kInvalidHeapEntry) {
    IRRef field = buf->getField(entry, fleft->op2() - 1);
    return field;
  }
  return NEXTFOLD;
}

// info(NEW k1 [...]) == k2 ==> k1 == k2
FOLDF(kfold_eqinfo_new) {
  // fleft is NEW instruction.
  fins->setOpcode(IR::kEQ);
  fins->setOp1(fleft->op1());
  return RETRYFOLD;
}

#undef fins
#undef fleft
#undef fright

TRef IRBuffer::optFold() {
  IR::Opcode op = fins()->opcode();
  IR::IRMode irmode = IR::mode(op);
  //  if (op == IR::kSLOAD) {
  //    return slots_.getAbsolute((int)fins()->op1());
  if (op == IR::kSAVE)
    return emit();

  IRRef ref = NEXTFOLD;
  if (flags_.get(kOptFold))
    ref = doFold();
  if (ref == NEXTFOLD) {
    if ((irmode & IR::IRM_S) == IR::IRM_N) {
      // If it's not a store/load/alloc, do CSE.
      return optCSE();
    } else
      return emit();
  }
  if (LC_LIKELY(ref >= MAX_FOLD))
    return TRef(ref, ir(ref)->t());
  if (ref == KLITFOLD)
    return literal(fold_.ins.type(), fold_.literal);
  if (ref == FAILFOLD)
    exit(44);
  LC_ASSERT(ref == DROPFOLD);
  return TRef();
}

IRRef IRBuffer::doFold() {
  IRRef ref;
  int retrycount = 0;
retry:
  ref = NEXTFOLD;
  IR::Opcode op = fins()->opcode();
  IR::IRMode mode = IR::mode(op);
  IRRef left = irmode_left(mode) == IR::IRMref ? fins()->op1() : 0;
  IRRef right = irmode_right(mode) == IR::IRMref ? fins()->op2() : 0;
  if (left != 0) {
    fold_.left = *ir(left);
  }
  if (right != 0) {
    fold_.right = *ir(right);
  }

  switch (op) {
  case IR::kADD:
    PATTERN(lit, lit, kfold_arith);
    PATTERN(ADD, lit, reassoc_int_arith);
    /// i + 0 ==> i
    PATTERN(any, lit, simplify_intadd_k);
    /// i + j ==> j + i, if i < j
    PATTERN(any, any, comm_swap);
    break;
  case IR::kSUB:
    /// constant folding
    PATTERN(lit, lit, kfold_arith);
    /// i - 0 ==> i
    /// i - k ==> i + (-k)
    PATTERN(any, lit, simplify_sub_k);
    /// 0 - i ==> -i
    PATTERN(lit, any, simplify_intsub_kleft);
    /// i - i ==> 0
    PATTERN(any, any, simplify_intsub);
    /// (i + j) - i ==> j
    /// (i + j) - j ==> i
    PATTERN(ADD, any, simplify_intsubadd_leftcancel);
    /// (i - j) - i ==> 0 - j
    PATTERN(SUB, any, simplify_intsubsub_leftcancel);
    /// i - (i - j) ==> j
    PATTERN(any, SUB, simplify_intsubsub_rightcancel);
    /// i - (i + j) ==> 0 - j
    /// i - (j + i) ==> 0 - j
    PATTERN(any, ADD, simplify_intsubadd_rightcancel);
    /// (x + y) - (x + z) ==> y - z
    /// (x + y) - (z + x) ==> y - z
    /// (y + x) - (x + z) ==> y - z
    /// (y + x) - (z + x) ==> y - z
    PATTERN(ADD, ADD, simplify_intsubaddadd_cancel);
    break;
  case IR::kEQINFO:
    // Info table guard on a static closure.
    PATTERN(lit, lit, kfold_eqinfo);
    // info(NEW k1 [...]) == k2 ==> k1 == k2
    PATTERN(NEW, lit, kfold_eqinfo_new);
    break;
  case IR::kHEAPCHK:
    /// heapchk N, heapchk M ==> heapchk (N+M)
    ref = foldHeapcheck();
    break;
  case IR::kEQ:
  case IR::kNE:
    PATTERN(lit, lit, kfold_cmp);
    break;
  case IR::kFLOAD:
    PATTERN(any, any, load_fwd);
    break;
  default:
    break;
  }

  if (ref == RETRYFOLD) {
    ++retrycount;
    if (LC_LIKELY(retrycount <= 100))
      goto retry;

    // In case the optimiser is looping, print a warning.
    if (retrycount == 101) {
      cerr << "\n>>> " COL_RED "ERROR" COL_RESET ": Optimiser looping.\n"
           << ">>> Please report this as a Lambdachine bug and include "
           << "the following:"
           << endl;
    }

    if (retrycount < 130) {
      cerr << ">>>   ";
      fold_.ins.debugPrint(cerr, REF_BASE);
      goto retry;
    } else { // retrycount >= 130
      // Emit whatever we have.  This protects against looping
      // transformation sequences, but not against
      // non-semantics-preserving transformations.
      cerr << ">>> End of error.\n" << endl;
      return NEXTFOLD;
    }
  }
  return ref;
}

_END_LAMBDACHINE_NAMESPACE
