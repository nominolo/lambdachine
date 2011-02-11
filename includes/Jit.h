#ifndef _LAMBDACHINE_JIT_H
#define _LAMBDACHINE_JIT_H

#include "Common.h"
#include "VM.h"
#include "IR.h"
#include "Bytecode.h"
#include "InfoTables.h"

#include <stdio.h>
#include <stdlib.h>

#define MAX_SLOTS    100

// -- Hot counters ---------------------------------------------------
// Hot counters are stored in a Capability, not globally, to avoid
// cache contention issues.
typedef u2 HotCount;

// Number of hot counter hash table entries (power of two)
#define HOTCOUNT_SIZE		64
#define HOTCOUNT_PCMASK		((HOTCOUNT_SIZE-1)*sizeof(HotCount))
// Initial value of hot counter.  Need data to figure out default.
// Will be customisable one day.
#define HOTCOUNT_DEFAULT        7

// -- JIT stuff ------------------------------------------------------

// The header structure describing the data in the snapshot map.
typedef struct _SnapShot {
  u2     mapofs; // Start offset into snapshot map.
  IRRef1 ref;    // First IR reference for this snapshot
  u1     nslots; // Number of valid slots;
  u1     nent;   // Number of compressed entries.
  u1     unused1;
  u1     unused2;
  u1     count;  // Number of taken exits for this snapshot.
} SnapShot;

typedef u4 SnapEntry;

// Construct SnapEntry from a slot and a tagged reference.
#define SNAP_TR(slot, tr) \
  (((SnapEntry)(slot) << 24) | ((SnapEntry)tref_ref(tr)))

#define snap_ref(sn)            ((sn) & 0xffff)
#define snap_slot(sn)           (cast(BCReg, ((sn) >> 24)))

typedef struct _Fragment {
  IRIns *ir;
  IRRef nins;  // Next IR instruction
  IRRef nk;    // Lowest IR literal
  IRRef nloop; // Reference to LOOP instruction (if any)

  u2 nsnap;    // Number of snapshots
  u2 nsnapmap; // Number of snapshot map elements
  SnapShot *snap;    // Snapshot array.
  SnapEntry *snapmap; // Snapshot map array.
  const BCIns *startpc; // needed for snapshot decoding
} Fragment;

typedef struct _FoldState {
  IRIns ins;
  IRIns left;
  IRIns right;
} FoldState;

typedef struct _JitState {
  Fragment cur;

  // Current VM state
  Thread *T;
  const BCIns *pc;
  FuncInfoTable *func;

  // Virtual/Recorder State
  TRef *base;
  TRef slot[MAX_SLOTS];
  BCReg baseslot;
  BCReg maxslot;
  TRef last_result;
  u4 flags;
  u4 mode;
  u4 framedepth;

  // IR Buffer.
  //
  // INVARIANTS:
  //   size = irmax - irmin;
  //   size != 0 ==> alloc_ptr =
  IRIns *irbuf;
  IRRef irmin;
  IRRef irmax;
  Word *kwords;
  u4 maxkwords;
  u4 nextkword;

  u1 needsnap;
  u1 mergesnap;
  u1 unused1;
  u1 unused2;

  // Snapshot buffer.
  //
  // The difference between this and `cur.snap`/`cur.snapmap` is that
  // these always point at the beginning of the buffer, while the
  // stuff inside `cur` might point somewhere in the middle.
  Word sizesnap;
  SnapShot *snapbuf;
  SnapEntry *snapmapbuf;
  Word sizesnapmap;

  const BCIns *startpc; // Address where recording was started.

  FoldState fold;
  IRRef1 chain[IR__MAX];
} JitState;

void initJitState(JitState *J);
LC_FASTCALL void startRecording(JitState *J, const BCIns *, Thread *, Word *base);
void recordSetup(JitState *J, Thread *T);
void finishRecording(JitState *J);
TRef LC_FASTCALL emitIR(JitState *J);
TRef foldIR(JitState *J);
LC_FASTCALL TRef optCSE(JitState *);
void optUnrollLoop(JitState *J);
void growIRBufferTop(JitState *J);
TRef emitLoadSlot(JitState *J, i4 slot);
int recordIns(JitState *J);

INLINE_HEADER TRef getSlot(JitState *J, int slot)
{
  return J->base[slot] ? J->base[slot] : emitLoadSlot(J, slot);
}

INLINE_HEADER void setSlot(JitState *J, int slot, TRef ref)
{
  //printf("Setting slot: %d (%ld) to %d\n",
  //       slot, &J->base[slot] - J->slot, (IRRef1)ref - REF_BIAS);
  J->base[slot] = ref;
  if (slot >= J->maxslot) J->maxslot = slot + 1;
}

// Put instruction in the folding slot.
INLINE_HEADER void setFoldIR(JitState *J, u2 ot, IRRef1 a, IRRef1 b)
{
  J->fold.ins.ot = ot;  J->fold.ins.op1 = a;  J->fold.ins.op2 = b;
}

// Emit instruction without optimisation.
INLINE_HEADER TRef emit_raw(JitState *J, u2 ot, IRRef1 a, IRRef1 b)
{
  setFoldIR(J, ot, a, b);
  return emitIR(J);
}

// Emit instruction with optimisations.
INLINE_HEADER TRef emit(JitState *J, u2 ot, IRRef1 a, IRRef1 b)
{
  setFoldIR(J, ot, a, b);
  return foldIR(J);
}

INLINE_HEADER void traceError(JitState *J, int n)
{
  exit(n);
}

#endif
