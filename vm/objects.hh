#ifndef _OBJECTS_H_
#define _OBJECTS_H_

#include "common.hh"
#include "bytecode.hh"

#include <iostream>

_START_LAMBDACHINE_NAMESPACE

typedef struct _Code {
  u1   framesize;               /* No. of local variables. */
  u1   arity;                   /* No. of function arguments.  */
  u2   sizecode;                /* No. of instructions in bytecode. */
  u2   sizelits;		/* No. of literals */
  u2   sizebitmaps;             /* No. of bitmaps (in multiples of `u2') */
  /* INVARIANT: framesize >= arity */
  Word  *lits;			/* Literals */
  u1    *littypes;              /* Types of literals.  See LitType. */
  BcIns *code;                  /* The bytecode followed by bitsets. */
  /* INVARIANT: code != NULL */
  void printLiteral(std::ostream &out, u4 litid) const;
} Code;

typedef enum {
  LIT_INT,    /* Word-sized integer literal */
  LIT_STRING, /* String literal (utf8-encoded) */
  LIT_CHAR,   /* Char literal, 32 bits */
  //  LIT_INT64,  /* Signed integer of at least 64 bits */
  LIT_WORD,   /* Word-sized unsigned integer */
  //  LIT_WORD64, /* Unsigned integer of at least 64 bits */
  LIT_FLOAT,  /* 32 bit floating point number */
  //  LIT_DOUBLE, /* 64 bit floating point number */
  LIT_CLOSURE, /* Reference to a (static) closure. */
  LIT_INFO,     /* Reference to an info table. */
  LIT_PC        /* Not actually used by bytecode, only by trace recorder. */
} LitType;

typedef union {
  struct {
    u2 ptrs;   // number of pointers
    u2 nptrs; // number of non-pointers
  } payload;
  u4 bitmap;   // bit pattern for describing stack frames
  u4 selectorOffset;
} ClosureInfo;

/* Closure flags. */
#define CF____ 0                /* No flags */
#define CF_HNF (1<<0)           /* Head normal form */
#define CF_THU (1<<1)           /* Thunk */
#define CF_IND (1<<2)           /* Indirection */

#define CTDEF(_) \
  _(INVALID_OBJECT, ___) \
  _(CONSTR,         HNF) \
  _(FUN,            HNF) \
  _(THUNK,          THU) \
  _(IND,            IND) \
  _(CAF,            THU) \
  _(PAP,            HNF) \
  _(AP_CONT,        HNF) \
  _(STATIC_IND,     IND) \
  _(UPDATE_FRAME,   ___) \
  _(BLACKHOLE,      ___)

#define DEF_CLOS_TY(name, flags) name,
typedef enum _ClosureType {
  CTDEF(DEF_CLOS_TY)
  N_CLOSURE_TYPES
} ClosureType;
#undef DEF_CLOS_TY

extern const u2 closureFlags[];

class InfoTable {
public:
  inline ClosureType type() const { return static_cast<ClosureType>(type_); }
  inline const char *name() const { return name_; }
  inline bool hasCode() const { return (kHasCodeBitmap & (1 << type())) != 0; }
  inline const ClosureInfo layout() const { return layout_; }
  inline u4 size() const { return size_; }
  void debugPrint(std::ostream&) const;
  static void printPayload(std::ostream&, u4 bitmap, u4 size);
private:
  void printPayload(std::ostream&) const;
  static const uint32_t kHasCodeBitmap =
    (1 << FUN) | (1 << THUNK) | (1 << CAF) | (1 << AP_CONT) |
    (1 << UPDATE_FRAME) | (1 << PAP);
  ClosureInfo layout_;
  u1 type_;       // closure type
  u1 size_;
  u2 tagOrBitmap_; // type == CONSTR_*: constructor tag
                  // type == FUN/THUNK: srt_bitmap
  const char *name_;
  friend class Loader;
  friend class MiscClosures;
  friend struct _Closure;
};

class ConInfoTable : public InfoTable {
};

class CodeInfoTable : public InfoTable {
public:
  inline const Code *code() const { return &code_; }
  void printCode(std::ostream&) const;
  void printLiteral(std::ostream&, u4 litid) const;
private:
  Code code_;
  friend class Loader;
  friend class MiscClosures;
};

typedef CodeInfoTable FuncInfoTable;
typedef CodeInfoTable ThunkInfoTable;

/* Used only during loading */
class FwdRefInfoTable : public InfoTable {
private:
  friend class Loader;
  void **next;
};

typedef struct _Closure Closure;

struct ClosureHeader {
public:
  inline InfoTable *info() const { return info_; }
  // private: <-- cannot use this, otherwise we get "not POD" warnings
  // when using offsetof(). Rather annoying.
  InfoTable *info_;
  friend struct _Closure;
  friend struct _PapClosure;
  friend class Loader;
};

struct _Closure {
public:
  ClosureHeader header_;
  Word payload_[];

  static inline void initHeader(Closure *c, InfoTable *info) {
    c->header_.info_ = info;
  }
  inline InfoTable *info() const { return header_.info(); }
  inline Word payload(int i) const { return payload_[i]; }
  inline void setInfo(InfoTable *info) { header_.info_ = info; }
  inline void setPayload(int i, Word value) { payload_[i] = value; }
  inline bool isIndirection() const {
    return closureFlags[info()->type()] & CF_IND;
  }
  inline u2 tag() const {
    LC_ASSERT(info()->type() == CONSTR);
    return info()->tagOrBitmap_;
  }
  inline bool isHNF() const {
    return closureFlags[info()->type()] & CF_HNF;
  }
};

typedef union {
  uint64_t combined;
  struct {
    uint32_t nargs_;
    uint32_t pointerMask_;
  };
} PapInfo;

typedef struct _PapClosure {
public:
  ClosureHeader header_;
  PapInfo info_;
  Closure *fun_;
  Word payload_[];
  inline void init(InfoTable *info, u4 ptrMask, u4 nargs, Closure *fun) {
    header_.info_ = info;
    info_.pointerMask_ = ptrMask;
    info_.nargs_ = nargs;
    fun_ = fun;
  }
  inline void setPayload(u4 i, Word value) { payload_[i] = value; }
  inline InfoTable *info() const { return header_.info(); }
  inline Word payload(u4 i) const { return payload_[i]; }
} PapClosure;

#define PAP_PAYLOAD_OFFSET   (offsetof(PapClosure, payload_))
#define PAP_FUNCTION_OFFSET  (offsetof(PapClosure, fun_))
#define PAP_INFO_OFFSET      (offsetof(PapClosure, info_))
LC_STATIC_ASSERT(is_word_aligned(PAP_PAYLOAD_OFFSET));
LC_STATIC_ASSERT(is_word_aligned(PAP_FUNCTION_OFFSET));
LC_STATIC_ASSERT(is_word_aligned(PAP_INFO_OFFSET));

bool isConstructor(Closure *cl);
void printClosure(std::ostream &out, Closure *cl, bool oneline);
void printClosureShort(std::ostream &out, Closure *cl);

_END_LAMBDACHINE_NAMESPACE

#endif /* _OBJECTS_H_ */
