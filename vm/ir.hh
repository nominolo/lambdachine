#ifndef _IR_H_
#define _IR_H_

#include "common.hh"
#include "vm.hh"

#include <vector>
#include <iostream>


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
  _(LT,      G,   ref, ref) \
  _(GE,      G,   ref, ref) \
  _(LE,      G,   ref, ref) \
  _(GT,      G,   ref, ref) \
  _(EQ,      G,   ref, ref) \
  _(NE,      G,   ref, ref) \
  _(EQRET,   G,   ref, ref) \
  _(EQINFO,  G,   ref, ref) \
  _(HEAPCHK, S,   lit, ___) \
   \
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
  _(KWORDHI, N,   cst, ___) \
  _(KBASEO,  N,   cst, ___) \
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
  _(NEG,     N,   ref, ___) \
  \
  _(FREF,    R,   ref, lit) \
  _(FLOAD,   L,   ref, ___) \
  _(SLOAD,   L,   lit, lit) \
  _(ILOAD,   L,   ref, ___) \
  _(RLOAD,   L,   ___, ___) \
  _(NEW,     A,   ref, lit) \
  _(FSTORE,  S,   ref, ref) \
  _(UPDATE,  S,   ref, ref) \
  _(SAVE,    S,   lit, lit)
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

// type name, printed string (len=3), color
#define IRTDEF(_) \
  _(UNKNOWN, "unk", GREY) \
  _(VOID,    "   ", NONE) \
  _(I64,     "i64", PRIM) \
  _(U64,     "u64", PRIM) \
  _(I32,     "i32", PRIM) \
  _(U32,     "u32", PRIM) \
  _(I16,     "i16", PRIM) \
  _(U16,     "u16", PRIM) \
  _(I8,      "i8 ", PRIM) \
  _(U8,      "u8 ", PRIM) \
  _(F64,     "f64", PRIM) \
  _(F32,     "f32", PRIM) \
  _(CHR,     "chr", PRIM) /* could be made = u32 */ \
  _(CLOS,    "cls", HEAP) /* pointer to a closure */ \
  _(INFO,    "inf", NONE) /* pointer to info table */ \
  _(PC,      "pc ", NONE) /* pointer to bytecode address */ \
  _(PTR,     "ptr", NONE) /* internal pointer */
/* 32 types in total allowed */

typedef enum {
#define IRTENUM(name, str, col) IRT_##name,
  IRTDEF(IRTENUM)
#undef IRTENUM

  // Flags
  IRT_MARK  = 0x20,  // Marker for various purposes
  IRT_ISPHI = 0x40,  // Used by register allocator
  IRT_GUARD = 0x80,  // Used by asm code generator

  // Masks
  IRT_TYPE = 0x1f,
  IRT_T    = 0xff
} IRType;

static const uint32_t kOpIsSigned =
  (1u << (int)IRT_I64) | (1u << (int)IRT_I32) |
  (1u << (int)IRT_I16) | (1u << (int)IRT_I8);

static const uint32_t kOpIsInteger =
  (1u << (int)IRT_CHR) |
  (1u << (int)IRT_U64) | (1u << (int)IRT_U32) |
  (1u << (int)IRT_U16) | (1u << (int)IRT_U8)  |
  (1u << (int)IRT_I64) | (1u << (int)IRT_I32) |
  (1u << (int)IRT_I16) | (1u << (int)IRT_I8);

inline bool isIntegerType(IRType t) {
  return kOpIsInteger & (1u << (int)t);
}

inline bool isSigned(IRType t) {
  return kOpIsSigned & (1u << (int)t);
}

static const uint32_t kOpIsFloat =
  (1u << (int)IRT_F32) | (1u << (int)IRT_F64);

inline bool isFloatType(IRType t) {
  return kOpIsFloat & (1 << (int)t);
}

#define IRT(o, t)      (cast(u4, ((o) << 8) | (t)))

class IRBuffer; // Defined below.

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

  // Properties of the instruction.
  typedef enum {
    IRMref = 0,
    IRMlit = 1,
    IRMcst = 2,
    IRMnone = 3,
    IRM___ = IRMnone,

    IRM_N = 0x00,  // normal, CSE ok
    IRM_R = IRM_N,
    IRM_C = 0x10,  // commutative
    IRM_A = 0x20,  // allocation (no CSE)
    IRM_L = 0x40,  // load
    IRM_S = 0x60,  // store
    IRM_G = 0x80   // guard
  } Mode;

  typedef uint8_t IRMode;
  typedef uint8_t Type;

  /// Returns the IR opcode of the instruction.
  inline Opcode opcode() { return (Opcode)data_.o; }

  /// Returns the instruction's type flags.
  inline uint8_t t() { return data_.t; }

  /// Returns the instruction's result type (excluding any flags).
  inline IRType type() { return (IRType)(data_.t & IRT_TYPE); }

  /// Returns the combined opcode and type flags fields.
  inline uint16_t ot() { return data_.ot; }

  /// Returns the first operand of the instruction.  It may represent
  /// other date depending on the instruction's mode.  @see
  /// IR::mode(Opcode).
  inline IRRef1 op1() { return data_.op1; }
  inline IRRef1 op2() { return data_.op2; }
  inline IRRef2 op12() { return data_.op12; }
  inline int32_t i32() { return data_.i; }
  inline uint32_t u32() { return data_.u; }
  inline uint8_t reg() const { return data_.r; }
  inline uint8_t spill() const { return data_.s; }

  /// Returns whether this instruction has been assigned a register or
  /// spill slot.
  bool hasRegOrSpill() const;

  inline bool isGuard() { return data_.t & IRT_GUARD; }

  inline void setOpcode(Opcode op) { data_.o = op; }
  inline void setT(uint8_t ty) { data_.t = ty; }
  inline void setOt(uint16_t ot) { data_.ot = ot; }
  inline void setOp1(IRRef1 op1) { data_.op1 = op1; }
  inline void setOp2(IRRef1 op2) { data_.op2 = op2; }
  inline void setPrev(IRRef1 prev) { data_.prev = prev; }
  inline void setReg(uint8_t r) { data_.r = r; }
  inline void setSpill(uint8_t s) { data_.s = s; }
  inline IRRef1 prev() { return data_.prev; }

  static inline IRMode mode(Opcode op) {
    LC_ASSERT(0 <= op && op <= k_MAX);
    return mode_[op];
  }

  static inline bool isCommutative(Opcode op) {
    return mode_[op] & IRM_C;
  }

  static inline bool hasSideEffect(Opcode op) {
    return mode_[op] & IRM_S;
  }

  static const char *regName(uint8_t reg, IRType ty);
  static void printIRRef(std::ostream &out, IRRef ref);
  void debugPrint(std::ostream &out, IRRef self) {
    debugPrint(out, self, NULL, false);
  }
  void debugPrint(std::ostream &out, IRRef self, IRBuffer *buf, bool regs);

private:
  IR(u1 opc, u1 ty, IRRef op1, IRRef op2, u2 prev) {
    data_.ot = (opc << 8) | ty;
    data_.op1 = op1;
    data_.op2 = op2;
    data_.prev = prev;
  }

  static uint8_t mode_[k_MAX + 1];
  static const char *name_[k_MAX + 1];

  friend class IRBuffer;

  IRIns data_;
};

#define IR_SAVE_FALLTHROUGH  0
#define IR_SAVE_LOOP  1
#define IR_SAVE_LINK  2

#define IR_SLOAD_DEFAULT 0
#define IR_SLOAD_INHERIT 1

#define irmode_left(mode) ((uint8_t)(mode) & 3)
#define irmode_right(mode) (((uint8_t)(mode) >> 2) & 3)

enum {
  REF_BIAS  = 0x8000,
  REF_BASE  = REF_BIAS,
  REF_FIRST = REF_BIAS + 1,
  REF_IND   = REF_BIAS - 1,
  REF_DROP  = 0xffff
};

inline bool irref_islit(IRRef ref) { return ref < REF_BIAS; }

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
  TRef() : raw_(0) { }
  inline IRRef1 ref() const { return (IRRef1)raw_; }
  inline IR::Type t() const { return (IR::Type)(raw_ >> 24); }
  inline bool isNone() const { return raw_ == 0; }
  inline bool isLiteral() const { return ref() < REF_BASE; }
  inline void markWritten() { raw_ |= kWritten; }
  inline bool isWritten() const { return raw_ & kWritten; }
  
  bool operator==(const TRef &t) const { return raw_ == t.raw_; }
  bool operator!=(const TRef &t) const { return raw_ != t.raw_; }
  operator IRRef1() { return (IRRef1)raw_; }

private:
  uint32_t raw_;

  static const uint32_t kWritten = 0x10000;

  friend class AbstractStack;
};

class SnapshotData;

typedef uint16_t SnapNo;

/// An entry in a snapshot.  See class Snapshot.
class SnapEntry {
public:
  inline int slot() const { return (int)(raw_ >> 16); }
  inline IRRef1 ref() const { return (IRRef1)raw_; }
private:
  SnapEntry(int s, IRRef1 r);
  inline explicit SnapEntry(uint32_t raw) : raw_(raw) {}
  uint32_t raw_;
  friend class Snapshot;
};

LC_STATIC_ASSERT(sizeof(SnapEntry) == sizeof(uint32_t));

inline SnapEntry::SnapEntry(int s, IRRef1 r) 
  : raw_(((uint32_t)s << 16) | (uint32_t)r) {}


/// A snapshot captures the state of the abstract stack at a
/// particular reference.
///
/// Snapshots are constructed only by the AbstractStack. The default
/// constructor simply constructs an empty snapshot with no reference.
///
/// To iterate over a snapshot's entries use this idiom:
///
///     for (Snapshot::MapRef sn = snap->begin();
///          sn < snap->end(); ++sn) {
///       IRRef1 ref = snapmap->slotRef(sn);
///       int slot = snapmap->slotId(sn);
///       ...
///     }
///
/// Note that `snapmap` must be the same one as the one used to
/// construct the snapshot.
///
/// Slots of a SnapEntry are relative to the base pointer at the
/// beginning of the trace.
///
class Snapshot {
public:
  /// Default constructor.
  Snapshot() : ref_(0), entries_(0), exitCounter_(HOT_SIDE_EXIT_THRESHOLD) {}

  /// Returns the snapshot reference.  The snapshot describes the
  /// state BEFORE the referenced instruction is executed.
  inline IRRef1 ref() const { return ref_; }

  /// The number of entries in this snapshot.
  inline int entries() const { return entries_; }

  /// The address of the base pointer when this snapshot was taken
  /// relative to the base at the start of the trace.
  inline int relbase() const { return relbase_; }

  // The size of the frame when the snapshot was taken.
  inline int framesize() const { return (int)framesize_; }

  typedef size_t MapRef;

  /// Returns pointer to the first snapshot entry.  Note, that this
  /// pointer is only valid if it is different from the pointer
  /// returned by end().
  MapRef begin() const { return mapofs_; }

  /// Returns a pointer one past the last entry for this snapshot.
  MapRef end() const { return mapofs_ + entries_; }

  void debugPrint(std::ostream&, SnapshotData *, SnapNo);

  // Warning: quite slow (only use for testing/debugging).
  IRRef1 slot(int n, SnapshotData *);

  BcIns *pc() const { return (BcIns*)pc_; }

  // Returns true if the side exit become hot.
  inline bool bumpExitCounter();

private:

  IRRef1 ref_;
  uint16_t mapofs_;
  int16_t relbase_; // base relative to trace entry base pointer
  uint8_t entries_;
  uint8_t framesize_;
  uint16_t exitCounter_;
  uint16_t unused;
  void *pc_;
  MCode *mcode_;
  friend class AbstractStack;
  friend class Assembler;  // Sets mcode_
};

inline bool Snapshot::bumpExitCounter() {
  --exitCounter_;
  return (exitCounter_ == 0);
}


class SnapshotData {
public:
  SnapshotData();
  inline int slotId(Snapshot::MapRef index) {
    return (int32_t)data_.at(index) >> 16;
  }
  inline IRRef1 slotRef(Snapshot::MapRef index) {
    return (IRRef1)data_.at(index);
  }
  void reset();
private:
  std::vector<uint32_t> data_;
  size_t index_;

  friend class AbstractStack;
  friend class Snapshot;
  friend class Jit;
};


class HeapSnapData {
public:
  HeapSnapData();
  ~HeapSnapData();
  void reset();
  inline IRRef1 at(int idx) { LC_ASSERT(idx < size_); return data_[idx]; }
private:
  inline int push_back(IRRef1 ref);
  //  void compact();
  void growTop();
  int reserve(int n);
  void set(int n, IRRef1 ref);

  IRRef1 *data_;
  size_t size_;
  size_t next_;

  friend class IRBuffer;
  friend class AbstractHeap;
};


inline int HeapSnapData::push_back(IRRef1 ref) {
  if (LC_UNLIKELY(next_ >= size_))
    growTop();
  int res = next_++;
  data_[res] = ref;
  return res;
}

inline int HeapSnapData::reserve(int n) {
  while (next_ + n >= size_) { growTop(); }
  int res = next_;
  next_ += n;
  return res;
}

inline void HeapSnapData::set(int n, IRRef1 ref) {
  LC_ASSERT(n < next_);  // Must call reserve first.
  data_[n] = ref;
}

class AbstractHeapEntry {
public:
  AbstractHeapEntry(IRRef1 ref, uint16_t size,
                    int ofs, int hpofs)
    : ref_(ref), size_(size), ofs_(ofs), hpofs_(hpofs) {}
  inline IRRef1 ref() const { return ref_; }
  inline int size() const { return size_; }
  inline int mapentry() const { return ofs_; }
  inline int hpOffset() const { return hpofs_; }
private:
  IRRef1 ref_;
  uint16_t size_;
  uint16_t ofs_;
  int16_t hpofs_;

  friend class AbstractHeap;
  friend class IRBuffer;
};


class AbstractHeap {
public:
  AbstractHeap();
  int newEntry(IRRef1 ref, int nfields);
  void reset();
  inline void heapCheck(int nwords) { reserved_ += nwords; }
  inline AbstractHeapEntry &entry(int n) { return entries_[n]; }
private:
  void grow();
  AbstractHeapEntry *entries_;
  uint32_t nentries_;
  uint32_t nextentry_;
  HeapSnapData data_;
  int reserved_;

  friend class IRBuffer;
};


class AbstractStack {
public:
  AbstractStack();
  ~AbstractStack();

  void reset(Word *base, Word *top);

  inline TRef get(int n) const {
    return slots_[base_ + n];
  }

  inline void clear(int n) {
    slots_[base_ + n] = TRef();
  }

  inline void set(int n, TRef value) {
    unsigned int slot = base_ + n;
    slots_[slot] = value;
    if (slot < low_) low_ = slot;
    if (slot > high_) high_ = slot;
  }

  inline unsigned int top() const {
    return top_ - base_;
  }

  inline int absolute(int n) const {
    return (base_ + n) - kInitialBase;
  }

  inline TRef getAbsolute(int n) const {
    return slots_[kInitialBase + n];
  }

  // Set the current frame.  Returns false in case of over-/underflow.
  bool frame(Word *base, Word *top);

  inline int baseOffset(Word *p) {
    return p - realOrigBase_;
  }

  inline Word *origBase() const { return realOrigBase_; }

  inline int highestSlot() const { return high_ - kInitialBase; }

  // TODO: Create snapshots.
  void snapshot(Snapshot *snap, SnapshotData *snapmap,
                IRRef1 ref, void *pc);

  void debugPrint(std::ostream &);

private:
  static const unsigned int kSlots = 500;
  static const unsigned int kInitialBase = 250;

  TRef *slots_;
  unsigned int base_; // current base
  unsigned int top_;  // current top
  unsigned int low_;  // lowest modified value
  unsigned int high_; // highest modified value
  Word *realOrigBase_;
};


typedef struct _FoldState {
  IR ins;
  IR left;
  IR right;
  uint64_t literal;
} FoldState;

class IRBuffer {
public:
  IRBuffer();
  ~IRBuffer();

  void reset(Word *base, Word *top);

  /// Reserve and return reference for next instruction to emit.
  /// Calling this function twice in a row returns adjacent
  /// references.
  IRRef nextIns();

  /// Reserve and return a reference for the next literal to emit.
  IRRef nextLit();

  inline TRef emit(uint8_t o, uint8_t t, IRRef1 op1, IRRef1 op2) {
    return emit(IRT(o, t), op1, op2);
  }

  inline TRef emit(uint16_t ot, IRRef1 op1, IRRef1 op2) {
    set(ot, op1, op2);
    return optFold();
  }

  /// Emit instruction in current fold state without passing it
  /// through the optimisation pipeline.
  inline TRef emitRaw(uint16_t ot, IRRef1 op1, IRRef1 op2) {
    set(ot, op1, op2);
    return emit();
  }

  inline TRef slot(int n) {
    TRef s = slots_.get(n);
    if (s.isNone()) {
      s = emitRaw(IRT(IR::kSLOAD, IRT_UNKNOWN), slots_.absolute(n),
                  IR_SLOAD_DEFAULT);
      slots_.set(n, s);
    }
    return s;
  }

  inline void setSlot(int n, TRef tr) {
    if (tr.ref() != 0) tr.markWritten();
    slots_.set(n, tr);
  }

  TRef literal(IRType ty, uint64_t lit);
  uint64_t literalValue(IRRef ref);

  /// A literal that represents a pointer into the stack.
  TRef baseLiteral(Word *p);
  TRef optFold();
  TRef optCSE();

  inline int size() { return (bufmax_ - bufmin_); }

  inline IR *ir(IRRef ref) {
    LC_ASSERT(bufmin_ <= ref && ref <= bufmax_);
    return &buffer_[ref];
  }

  void debugPrint(std::ostream&, int traceNo);

  static const int kOptCSE = 0;
  static const int kOptFold = 1;
  static const int kRegsAllocated = 16;

  inline void enableOptimisation(int optId) { flags_.set(optId); }
  inline void disableOptimisation(int optId) { flags_.clear(optId); }
  
  void snapshot(IRRef ref, void *pc);
  SnapNo snapshot(void *pc);
  inline Snapshot &snap(SnapNo n) {
    LC_ASSERT(n < snaps_.size());
    return snaps_.at(n);
  }
  inline SnapNo numSnapshots() const { return snaps_.size(); }

  SnapshotData *snapmap() { return &snapmap_; }

  inline void emitHeapCheck(int nwords) {
    emit(IR::kHEAPCHK, IRT_VOID|IRT_GUARD, nwords, 0);
    heap_.heapCheck(nwords);
  }

  typedef int HeapEntry;
  TRef emitNEW(IRRef1 itblref, int nfields, HeapEntry *entry1);
  void setHeapOffsets();
  inline int numFields(HeapEntry entry);
  inline void setField(HeapEntry entry, int field, IRRef1 ref);
  static const HeapEntry kInvalidHeapEntry = -1;
  inline HeapEntry getHeapEntry(IRRef ref);
  inline IRRef1 getField(HeapEntry entry, int field);

  inline bool regsAllocated() { return flags_.get(kRegsAllocated); }
  inline void setPC(void *pc) { pc_ = pc; }
private:
  inline void setRegsAllocated() { flags_.set(kRegsAllocated); }

  void growTop();
  void growBottom();
  TRef emit(); // Emit without optimisation.

  IRRef foldHeapcheck();

  IRRef doFold();

  inline void set(uint16_t ot, IRRef1 op1, IRRef op2) {
    fold_.ins.setOt(ot);
    fold_.ins.setOp1(op1);
    fold_.ins.setOp2(op2);
  }

  inline IR *fins() {
    return &fold_.ins;
  }

  void *pc_;                    // Current PC
  IR *realbuffer_;
  IR *buffer_;  // biased
  Flags32 flags_;
  IRRef bufmin_;  // Lowest IR constant
  IRRef bufmax_;  // Next IR instruction
  IRRef bufstart_;
  IRRef bufend_;
  size_t size_;
  FoldState fold_;
  IRRef1 chain_[IR::k_MAX];
  AbstractStack slots_;
  SnapshotData snapmap_;
  std::vector<Snapshot> snaps_;
  AbstractHeap heap_;

  IRRef stopins_;
  typedef uint16_t InheritedSlotInfo;
  InheritedSlotInfo parentmap_[200];
  Fragment *parent_;
  int32_t entry_relbase_;
  // no. of inherited slots = stopins_ - REF_FIRST.

  friend class Jit;
  friend class Assembler;
};

inline IRRef IRBuffer::nextIns() {
  IRRef ref = bufmax_;
  if (LC_UNLIKELY(ref >= bufend_)) growTop();
  bufmax_ = ref + 1;
  return ref;
}

inline IRRef IRBuffer::nextLit() {
  IRRef ref = bufmin_;
  if (LC_UNLIKELY(ref <= bufstart_)) growBottom();
  bufmin_ = --ref;
  return ref;
}

inline
IRBuffer::HeapEntry IRBuffer::getHeapEntry(IRRef ref) {
  IR *ins = ir(ref);
  if (ins->opcode() != IR::kNEW)
    return kInvalidHeapEntry;
  return (HeapEntry)ins->op2();
}

inline
void IRBuffer::setField(HeapEntry entry, int field, IRRef1 ref) {
  heap_.data_.set(heap_.entries_[entry].mapentry() + field, ref);
}

inline IRRef1 IRBuffer::getField(HeapEntry entry, int field) {
  return heap_.data_.at(heap_.entries_[entry].mapentry() + field);
}

inline int IRBuffer::numFields(HeapEntry entry) {
  return heap_.entries_[entry].size();
}

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
