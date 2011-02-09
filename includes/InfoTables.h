#ifndef _LAMBDACHINE_INFOTABLES_H
#define _LAMBDACHINE_INFOTABLES_H

#include "Common.h"
#include "Bytecode.h"

typedef struct {
  u2   framesize;               /* No. of local variables. */
  u2   arity;                   /* No. of function arguments.  */
  u2   sizecode;                /* No. of instructions in bytecode. */
  u2   sizelits;		/* No. of literals */
  /* INVARIANT: framesize >= arity */
  Word  *lits;			/* Literals */
  u1    *littypes;              /* Types of literals.  See LitType. */
  BCIns *code;                  /* The bytecode. */
  /* INVARIANT: code != NULL */
} LcCode;

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
  LIT_INFO     /* Reference to an info table. */
} LitType;

typedef struct _InfoTable InfoTable;
typedef InfoTable LcInfoTable;

typedef struct _ClosureHeader {
  const InfoTable* info;
} ClosureHeader;

typedef struct _Closure {
  ClosureHeader header;
  Word          payload[FLEXIBLE_ARRAY];
} Closure;

typedef struct _PapClosure {
  ClosureHeader header;
  HalfWord      arity;
  HalfWord      nargs;
  Closure      *fun;
  Word          payload[FLEXIBLE_ARRAY];
} PapClosure;

/*  
inline InfoTable *getInfo(Closure *cl) { return cl->header.info; }
*/
#define getInfo(cl)    (cast(Closure*,cl)->header.info)
#define setInfo(cl, i) (cast(Closure*,cl)->header.info = (i))

#define getTag(cl)    (getInfo(cl)->tagOrBitmap)

/* 

Info Tables
-----------
 
*/

typedef union {
  struct {
    u2 ptrs;   // number of pointers
    u2 nptrs; // number of non-pointers
  } payload;
  u4 bitmap;   // bit pattern for describing stack frames
  u4 selectorOffset;
} ClosureInfo;

struct _InfoTable {
  ClosureInfo layout;
  u2 type;       // closure type
  u2 tagOrBitmap; // type == CONSTR_*: constructor tag
                  // type == FUN/THUNK: srt_bitmap
};

#define DEF_INFO_TABLE(ty_, tag_, ptrs_, nptrs_) \
  { .layout = { .payload = { .ptrs = (ptrs_), .nptrs = (nptrs_) } }, \
       .type = (ty_), .tagOrBitmap = (tag_) }

#define DEF_CLOSURE(itbl, payload_) \
  { .header.info = cast(InfoTable*,itbl), .payload = payload_ }

typedef struct _FuncInfoTable {
  InfoTable i;
  char     *name;
  LcCode    code;
} FuncInfoTable;

typedef FuncInfoTable ThunkInfoTable;

typedef struct _ConInfoTable {
  InfoTable i;
  char     *name;
} ConInfoTable;

typedef ConInfoTable PapInfoTable;

/* Used only during loading */
typedef struct _FwdRefInfoTable {
  InfoTable i;
  void **next;
} FwdRefInfoTable;

#define getFInfo(cl)  (cast(FuncInfoTable*,getInfo(cl)))


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
  _(STATIC_IND,     IND) \
  _(UPDATE_FRAME,   ___) \
  _(BLACKHOLE,      ___)

#define DEF_CLOS_TY(name, flags) name,
enum {
  CTDEF(DEF_CLOS_TY)
  N_CLOSURE_TYPES
} ClosureType;
#undef DEF_CLOS_TY

extern u2 closure_flags[];

#define closureFlags(cl) (closure_flags[getInfo(cl)->type])

#define closure_HNF(cl)    ( closureFlags(cl) & CF_HNF )
#define closure_THUNK(cl)  ( closureFlags(cl) & CF_THU )
#define closure_IND(cl)    ( closureFlags(cl) & CF_IND )



#endif /* LC_INFOTABLES_H */
