#ifndef _LAMBDACHINE_INFOTABLES_H
#define _LAMBDACHINE_INFOTABLES_H

#include "Common.h"
#include "Bytecode.h"

/*

Current Bitmap Story (likely to change)
---------------------------------------

Every return point is preceded by an offset into the pointer bitmask.
These are multiples of 16 bits; the highest bit serves as a
continuation bit; the LSB is the first bit of the sequence.  If it is
set another 16 bit follows.  E.g., the bit sequence

    1100 1010 0100 0100 01

is first split into chunks of 15 bits

    1100 1010 0100 010  |  0010

end then encoded in little endian:

    0xa253 0x0004

There are two such bitmasks.  The first one is pointer mask, the
second the liveness mask.  The pointer mask must be a subset of the
liveness mask.  If a bit is set, then the corresponding register
contains a pointer and/or is live at this point.  For example, the two
bitmasks

    0x0005 0x0007

are interpreted as registers r0 and r2 contain a pointer, and
registers r0, r1, and r2 contain live values.


For heap objects, the bitmask describing the pointerhood of each argument
is located in the `layout.bitmap` field of the info table.  These bitmaps
are currently fixed-size 32 bit values.  So, for now, we don't support
heap objects with more than 32 fields.

For function closures we need both a bitmap for the free variables as
well as for the function arguments (used by PAPs and the JIT).  The
latter are encoded the same way as return point bitmasks and are at
the location immediately following the functions bytecode
instructions.  I.e., if `i` is a pointer to the functions info table
then it can be accessed as:

    i->code.code[i->code.sizecode]

*/

typedef struct {
  u2   framesize;               /* No. of local variables. */
  u2   arity;                   /* No. of function arguments.  */
  u2   sizecode;                /* No. of instructions in bytecode. */
  u2   sizelits;		/* No. of literals */
  u2   sizebitmaps;             /* No. of bitmaps (in multiples of `u2') */
  /* INVARIANT: framesize >= arity */
  Word  *lits;			/* Literals */
  u1    *littypes;              /* Types of literals.  See LitType. */
  BCIns *code;                  /* The bytecode followed by bitsets. */
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
  LIT_INFO,     /* Reference to an info table. */
  LIT_PC        /* Not actually used by bytecode, only by trace recorder. */
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

typedef struct _IndClosure {
  ClosureHeader header;
  Closure      *indirectee;
} IndClosure;

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
  u1 type;       // closure type
  u1 size;
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
typedef FuncInfoTable ApContInfoTable;
typedef FuncInfoTable ApInfoTable;

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
  _(AP_CONT,        HNF) \
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


// --- Bitmasks -------------------------------------------

// Return a pointer to the bitmask associated with the current PC.
// May return NULL.  In that case, the bitmap is to be extracted from
// the stack.
INLINE_HEADER
const u2 *
getPointerMask(const BCIns *next_pc)
{
  const BCIns *p0 = &next_pc[-1];
  u4 offset = (u4)(*p0);
  if (offset != 0)
    return (const u2*)((u1*)p0 + offset);
  else
    return NULL;
}

extern const u2 emptyBitmask[2];         /* Defined in Record.c */

INLINE_HEADER
const u2 *
getParamPointerMask(const FuncInfoTable *info)
{
  if (info->i.type == FUN)
    return (const u2 *)&info->code.code[info->code.sizecode];
  else
    return &emptyBitmask[0];
}

INLINE_HEADER
const u2 *
skipBitmap(const u2 *p)
{
  while (*p++ & 0x8000) ;
  return p;
}

INLINE_HEADER
const u2 *
getLivenessMask(const BCIns *next_pc)
{
  return skipBitmap(getPointerMask(next_pc));
}

// Macro for iterating over a variable-length bitmask.
//
// The last three arguments are the three components of a
// "for"-loop.  The first three parameters must be variable names.
//
//  - mask_ptr .. must be of type u2* and points to the beginning of the
//                bitmask.  Its value is being updated by the loop.
//  - mask_var .. contains the contents of the mask.  The lowest bit
//                always corresponds to the current value.  It must be of
//                type u2.
//  - mask_ctr .. is an internal counter (of type int)
//
// Example (prints all the set bits):
//
//   FOR_MASK(liveout, mask, j, i = 1, i <= t, i++) {
//     if (mask & 1) {
//       printf("%d\n", i);
//     }
//   }
//
#define FOR_MASK(mask_ptr, mask_var, mask_ctr, init, cond, post) \
  for ((init), (mask_var = *mask_ptr++, mask_ctr = 0);           \
       (cond);                                                   \
       (post), (mask_ctr = (mask_ctr < 14) ?                     \
                   (mask_var >>= 1, mask_ctr + 1) :     \
                   (mask_var = *mask_ptr++, 0)))


#endif /* LC_INFOTABLES_H */
