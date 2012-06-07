#ifndef _OBJECTS_H_
#define _OBJECTS_H_

#include "common.hh"
#include "bytecode.hh"

#include <iostream>

_START_LAMBDACHINE_NAMESPACE

typedef struct {
  u2   framesize;               /* No. of local variables. */
  u2   arity;                   /* No. of function arguments.  */
  u2   sizecode;                /* No. of instructions in bytecode. */
  u2   sizelits;		/* No. of literals */
  u2   sizebitmaps;             /* No. of bitmaps (in multiples of `u2') */
  /* INVARIANT: framesize >= arity */
  Word  *lits;			/* Literals */
  u1    *littypes;              /* Types of literals.  See LitType. */
  BcIns *code;                  /* The bytecode followed by bitsets. */
  /* INVARIANT: code != NULL */
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

class InfoTable {
public:
  inline ClosureType type() const { return static_cast<ClosureType>(type_); }
  inline const char *name() const { return name_; }
  inline bool hasCode() const { return (kHasCodeBitmap & (1 << type())) != 0; }
  inline const ClosureInfo layout() const { return layout_; }
  inline const u4 size() const { return size_; }
  void debugPrint(std::ostream&) const;
private:
  void printPayload(std::ostream&) const;
  static const uint32_t kHasCodeBitmap =
    (1 << FUN) | (1 << THUNK) | (1 << CAF) | (1 << AP_CONT) |
    (1 << UPDATE_FRAME);
  ClosureInfo layout_;
  u1 type_;       // closure type
  u1 size_;
  u2 tagOrBitmap_; // type == CONSTR_*: constructor tag
                  // type == FUN/THUNK: srt_bitmap
  const char *name_;
  friend class Loader;
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

class ClosureHeader {
public:
  inline InfoTable *info() const { return info_; }
private:
  InfoTable *info_;
  friend struct _Closure;
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
};

void printClosure(std::ostream &out, Closure *cl, bool oneline);

_END_LAMBDACHINE_NAMESPACE

#endif /* _OBJECTS_H_ */
