#ifndef _IR_H_
#define _IR_H_

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

typedef u2 IRRef1;              /* One stored reference */
typedef u4 IRRef2;            /* Two stored references */
typedef u4 IRRef;               /* Used to pass around references */

// A side effect of this design is that we have to use two IR instructions
// per store, because a store takes three arguments (base pointer,
// offset, and value).  A store is therefore encoded as:
//
//     ref  FREF   base offs   ; reference to a field
//     void FSTORE ref value
//
// Flags:
//   N .. normal
//   C .. commutative
//   G .. guard
//   A .. alloc
//
// Operand Types:
//   ref .. IR reference
//   lit .. 16 bit unsigned literal
//   cst .. 32/64bit constant (pointer, int)
//   none .. unised operand
//
#define IRDEF(_) \
  _(NOP,     N,   ___, ___) \
  _(BASE,    N,   lit, lit) \
  _(FRAME,   S,   lit, lit) \
  _(RET,     S,   lit, lit) \
  _(LOOP,    N,   ___, ___) \
  _(PHI,     N,   ref, ref) \
  _(RENAME,  S,   ref, lit) \
  \
  _(KINT,    N,   cst, ___) \
  _(KWORD,   N,   cst, ___) \
  _(KBASEO,  N,   cst, ___) \
  \
  _(LT,      G,   ref, ref) \
  _(GE,      G,   ref, ref) \
  _(LE,      G,   ref, ref) \
  _(GT,      G,   ref, ref) \
  _(EQ,      G,   ref, ref) \
  _(NE,      G,   ref, ref) \
  _(EQRET,   G,   ref, ref) \
  _(EQINFO,  G,   ref, ref) \
  _(HEAPCHK, G,   cst, ___) \
   \
  _(BNOT,    N,   ref, ___) \
  _(BAND,    C,   ref, ref) \
  _(BOR,     C,   ref, ref) \
  _(BXOR,    C,   ref, ref) \
  _(BSHL,    N,   ref, ref) \
  _(BSHR,    N,   ref, ref) \
  _(BSAR,    N,   ref, ref) \
  _(BROL,    N,   ref, ref) \
  _(BROR,    N,   ref, ref) \
  \
  _(ADD,     C,   ref, ref) \
  _(SUB,     N,   ref, ref) \
  _(MUL,     C,   ref, ref) \
  _(DIV,     N,   ref, ref) \
  _(REM,     N,   ref, ref) \
  \
  _(FREF,    R,   ref, lit) \
  _(FLOAD,   L,   ref, ___) \
  _(SLOAD,   L,   lit, lit) \
  _(ILOAD,   L,   ref, ___) \
  _(RLOAD,   L,   ___, ___) \
  _(NEW,     A,   ref, lit) \
  _(FSTORE,  S,   ref, ref) \
  _(UPDATE,  S,   ref, ref) \
  _(SAVE,    S,   lit, ___)
/*
 * LuaJIT IR
 *
 * LSB                           MSB
 *
 *    16      16     8   8   8   8
 * +-------+-------+---+---+---+---+
 * |  op1  |  op2  | t | o | r | s |
 * +-------+-------+---+---+---+---+
 * |  op12/i/gco   |  ot   | prev  |
 * +---------------+-------+-------+
 *        32          16      16
 *
 *   o .. opcode
 *   t .. IR type + flags
 *   r .. register allocation
 *   s .. spill slot allocation
 *
 *   prev .. Points to previous instruction with same opcode (or 0).
 *           Used for fast CSE.  Only valid before register allocation.
 */

typedef union _IRInsRaw {
  struct {
    LC_ENDIAN_LOHI(
      IRRef1 op1;               // First operand.
    , IRRef1 op2;               // Second operand
    )
    u2 ot;                      // Opcode + type
    IRRef1 prev;                // Previous instruction of same opcode
  };
  struct {
    IRRef2 op12;                // Unified operands.  (op1 is low)
    LC_ENDIAN_LOHI(
      u1 t;
    , u1 o;
    )
    LC_ENDIAN_LOHI(
      u1 r;
    , u1 s;
    )
  };

  // TODO: I think we need more LC_ENDIAN_LOHI to make sure that these
  // overlap with op1/2, not ot/prev

  i4 i;   // A literal 32bit integer
  u4 u;   // Index into J->kwords
} IRIns;

class IR {
public:
  IR() : data_() {}
  IR(IRIns d) : data_(d) { }
  ~IR() {}
  // Opcodes
  typedef enum {
#define IRENUM(name,flags,arg1,arg2) k##name,
    IRDEF(IRENUM)
#undef IRENUM
    k_MAX
  } Opcode;

  typedef enum {
    IRM_N = 0x00,  // normal, CSE ok
    IRM_R = IRM_N,
    IRM_C = 0x10,  // commutative
    IRM_A = 0x20,  // allocation (no CSE)
    IRM_L = 0x40,  // load
    IRM_S = 0x60,  // store
    IRM_G = 0x80   // guard
  } Mode;

  typedef uint8_t Type;

  inline Opcode opcode() { return (Opcode)data_.o; }
  inline Type t() { return data_.t; }
  inline uint16_t ot() { return data_.ot; }
  inline IRRef1 op1() { return data_.op1; }
  inline IRRef1 op2() { return data_.op2; }
  inline IRRef2 op12() { return data_.op12; }

  inline void setOpcode(Opcode op) { data_.o = op; }
  inline void setT(uint8_t ty) { data_.t = ty; }
  inline void setOt(uint16_t ot) { data_.ot = ot; }
  inline void setOp1(IRRef1 op1) { data_.op1 = op1; }
  inline void setOp2(IRRef1 op2) { data_.op2 = op2; }
  inline void setPrev(IRRef1 prev) { data_.prev = prev; }
  inline IRRef1 prev() { return data_.prev; }

  static inline bool isCommutative(Opcode op) {
    return mode[op] & IRM_C;
  }

private:
  IR(u1 opc, u1 ty, IRRef op1, IRRef op2, u2 prev) {
    data_.ot = (opc << 8) | ty;
    data_.op1 = op1;
    data_.op2 = op2;
    data_.prev = prev;
  }

  static uint8_t mode[k_MAX + 1];

  friend class IRBuffer;

  IRIns data_;
};

// A tagged reference
// Tagged IR references (32 bit).
//
// +-------+-------+---------------+
// |  irt  | flags |      ref      |
// +-------+-------+---------------+
//
// The tag holds a copy of the IRType and speeds up IR type checks.
//
class TRef {
public:
  TRef(IRRef1 ref, IR::Type type) {
    raw_ = (uint32_t)ref + ((uint32_t)type << 24);
  }
  inline IRRef1 ref() const { return (IRRef1)raw_; }
  inline IR::Type t() const { return (IR::Type)(raw_ >> 24); }
private:
  uint32_t raw_;
};

enum {
  REF_BIAS  = 0x8000,
  REF_BASE  = REF_BIAS,
  REF_FIRST = REF_BIAS + 1,
  REF_DROP  = 0xffff
};

typedef enum {
  IRT_UNK,
  IRT_VOID,  // No result
  IRT_I32,
  IRT_U32,
  IRT_CHAR,
  IRT_F32,

  IRT_CLOS,  // Pointer to a closure
  IRT_INFO,  // Pointer to an info table
  IRT_PC,    // Program counter
  IRT_PTR,   // Other pointer

  // Flags
  IRT_MARK  = 0x20,  // Marker for various purposes
  IRT_ISPHI = 0x40,  // Used by register allocator
  IRT_GUARD = 0x80,  // Used by asm code generator

  // Masks
  IRT_TYPE = 0x1f,
  IRT_T    = 0xff
} IRType;

#define IRT(o, t)      (cast(u4, ((o) << 8) | (t)))

typedef struct _FoldState {
  IR ins;
  IR left;
  IR right;
} FoldState;

class IRBuffer {
public:
  IRBuffer();
  ~IRBuffer();

  inline IRRef nextIns() {
    IRRef ref = bufmax_;
    if (LC_UNLIKELY(ref >= bufend_)) growTop();
    bufmax_ = ref + 1;
    return ref;
  }

  inline TRef emit(uint16_t ot, IRRef1 op1, IRRef op2) {
    set(ot, op1, op2);
    return optFold();
  }
  
  /// Emit instruction in current fold state without passing it
  /// through the optimisation pipeline.
  inline TRef emitRaw(uint16_t ot, IRRef1 op1, IRRef op2) {
    set(ot, op1, op2);
    return emit();
  }

  TRef optFold();
  TRef optCSE();

  inline int size() { return (bufmax_ - bufmin_) - 1; }

  inline IR *ir(IRRef ref) {
    LC_ASSERT(bufmin_ < ref && ref <= bufmax_);
    return &buffer_[ref];
  }

private:
  void growTop();
  TRef emit(); // Emit without optimisation.
  
  inline void set(uint16_t ot, IRRef1 op1, IRRef op2) {
    fold_.ins.setOt(ot);
    fold_.ins.setOp1(op1);
    fold_.ins.setOp2(op2);
  }

  inline IR *fins() {
    return &fold_.ins;
  }

  IR *realbuffer_;
  IR *buffer_;  // biased
  IRRef bufmin_;
  IRRef bufmax_;
  IRRef bufstart_;
  IRRef bufend_;
  size_t size_;
  FoldState fold_;
  IRRef chain_[IR::k_MAX];
};

// Can invert condition by toggling lowest bit.
LC_STATIC_ASSERT((IR::kLT ^ 1) == IR::kGE);
LC_STATIC_ASSERT((IR::kGT ^ 1) == IR::kLE);
LC_STATIC_ASSERT((IR::kEQ ^ 1) == IR::kNE);
// Order of comparison operations matters.  Same is enforced for bytecode.
LC_STATIC_ASSERT((IR::kLT & 1) == 0);
LC_STATIC_ASSERT((IR::kLT + 2) == IR::kLE);
LC_STATIC_ASSERT((IR::kLE + 2) == IR::kEQ);

_END_LAMBDACHINE_NAMESPACE

#endif /* _IR_H_ */
