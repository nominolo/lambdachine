#include "Common.h"
#include "IR.h"

#include <stdlib.h>
#include <stdio.h>

#define MAX_SLOTS    100

typedef u4 BCReg;

typedef enum {
  IRT_ClosPtr,
  IRT_InfoTablePtr,
  IRT_Int,   // Int#
  IRT_Char,  // Char#
  IRT_Bool,  // Bool#
  IRT_Int8,
  IRT_Int16,
  IRT_Int32,
  IRT_Int64,
  IRT_Word8,
  IRT_Word16,
  IRT_Word32,
  IRT_Word64,
  IRT_Double,
  IRT_Float,
} IRType;

typedef struct _Fragment {
  IRIns *ir;
  IRRef nins;
  
} Fragment;

typedef struct _FoldState {
  IRIns ins;
  IRIns left;
  IRIns right;
} FoldState;

typedef struct _JitState {
  Fragment cur;
  TRef *base;
  TRef slot[MAX_SLOTS];
  BCReg baseslot;
  BCReg maxslot;
  Thread *T;

  IRIns *irbuf;
  IRRef irmin;
  IRRef irmax;
  const BCIns *startpc;

  FoldState fold;
  IRRef1 chain[IR__MAX];
} JitState;

TRef LC_FASTCALL emitIR(JitState *J);
TRef foldIR(JitState *J);
void growIRBufferTop(JitState *J);

INLINE_HEADER void setFoldIR(JitState *J, u2 ot, IRRef1 a, IRRef1 b)
{
  J->fold.ins.ot = ot;  J->fold.ins.op1 = a;  J->fold.ins.op2 = b;
}

INLINE_HEADER TRef emit_raw(JitState *J, u2 ot, IRRef1 a, IRRef1 b)
{
  setFoldIR(J, ot, a, b);
  return emitIR(J);
}

INLINE_HEADER TRef emit(JitState *J, u2 ot, IRRef1 a, IRRef1 b)
{
  setFoldIR(J, ot, a, b);
  return foldIR(J);
}

static TRef loadSlot(JitState *J, i4 slot) {
  
}

// Address of reference
#define IR(ref)     (&J->cur.ir[(ref)])
// The instruction currently being optimised
#define foldIns     (&J->fold.ins)

INLINE_HEADER IRRef nextIns(JitState *J)
{
  IRRef ref = J->cur.nins;
  if (LC_UNLIKELY(ref >= J->irmax)) growIRBufferTop(J);
  J->cur.nins = ref + 1;
  return ref;
}

/* Emit current IR instruction without any optimisation. */
TRef LC_FASTCALL emitIR(JitState *J)
{
  IRRef ref = nextIns(J);
  IRIns *ir = IR(ref);
  IROp op = foldIns->o;
  // Link into per-opcode chain.
  ir->prev = J->chain[op];
  J->chain[op] = (IRRef1)ref;
  ir->o = op;
  ir->op1 = foldIns->op1;
  ir->op2 = foldIns->op2;
  return ref; // TODO: Maybe add type info
}





void growIRBufferTop(JitState *J)
{
  fprintf(stderr, "TODO: growIRBufferTop\n");
  exit(1);
}
