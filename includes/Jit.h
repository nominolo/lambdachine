#ifndef _LAMBDACHINE_JIT_H
#define _LAMBDACHINE_JIT_H

#include "Common.h"
#include "VM.h"
#include "IR.h"
#include "Bytecode.h"
#include "InfoTables.h"
#include "Opts.h"

#include <stdio.h>
#include <stdlib.h>

#define MAX_SLOTS    100
#define MAX_TRACE_LENGTH 2000

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
  u1     count;  // Number of taken exits for this snapshot.
} SnapShot;

typedef u4 SnapEntry;

// Construct SnapEntry from a slot and a tagged reference.
#define SNAP_TR(slot, tr) \
  (((SnapEntry)(slot) << 24) | ((SnapEntry)tref_ref(tr)))

#define snap_ref(sn)            ((sn) & 0xffff)
#define snap_slot(sn)           (cast(BCReg, ((sn) >> 24)))

/* 
 * Heap Info
 * ---------
 *
 * The heap info is an abstraction for the contents of the heap.  Heap
 * info entries are initialised when an object is allocated and will
 * not be modified after that.  To support UPDATE operations, a special field 
 * is reserved to point to the new new value if needed.
 *
 * 
 */

typedef IRRef1 HeapEntry;

#define heap_ref(hr)            (hr)

typedef struct _HeapInfo {
  u2 mapofs;
  IRRef1 ref; // First reference to heap object
  u1 nfields; // Total number of fields
  u1 nent;    // Number of `HeapEntry`s used
  u1 compact; // non-zero if fields are in order.
  u1 loop;
  IRRef1 ind; // Points to the new object after an UPDATE.
  u2 dfs;
  u2 scc;
} HeapInfo;

/* Snapshot and exit numbers. */
typedef u4 SnapNo; /* Snapshot numbers */
typedef u4 ExitNo; /* Trace exit numbers */

/* Machine code types. */
typedef u1 MCode;  /* Type for storing Machine code */
typedef u4 MSize;  /* Machine code size */

/* Fragments */
typedef u2 FragmentId;

typedef struct _Fragment {
  IRIns *ir;
  IRRef nins;  // Next IR instruction
  IRRef nk;    // Lowest IR literal
  IRRef nloop; // Reference to LOOP instruction (if any)
  u2 nphis;    // Number of PHI nodes (only needed by IR interpreter)

  Word *kwords; // Constant words
  u4 nkwords;

  u2 nsnap;    // Number of snapshots
  u2 nsnapmap; // Number of snapshot map elements
  SnapShot *snap;    // Snapshot array.
  SnapEntry *snapmap; // Snapshot map array.

  const BCIns *startpc; // needed for snapshot decoding
  BCIns orig;  // Original instruction at trace start

  u2 nheap;
  u2 nheapmap;
  HeapInfo *heap;
  HeapEntry *heapmap;
  u4 framesize; // Number of frame slots used by this fragment (before RA)

  MCode *mcode;  // Machine code for the trace
  MSize szmcode; // Size of machine code
} Fragment;

/* Fold state is used to fold instructions on-the-fly. */
typedef struct _FoldState {
  IRIns ins;
  IRIns left;
  IRIns right;
} FoldState;

/* Optimization parameters and their defaults. Length is a char in octal! */
#define JIT_PARAMDEF(_) \
  _(\011, enableasm,	   0)	/* Generate machine code for traces. */ \
  /* Size of each machine code area (in KBytes). */ \
  _(\011, sizemcode,	64) \
  /* Max. total size of all machine code areas (in KBytes). */ \
  _(\010, maxmcode,	512) \
  /* End of list. */

enum {
#define JIT_PARAMENUM(len, name, value)	JIT_P_##name,
JIT_PARAMDEF(JIT_PARAMENUM)
#undef JIT_PARAMENUM
  JIT_P__MAX
};
#define JIT_PARAMSTR(len, name, value)	#len #name
#define JIT_P_STRING	JIT_PARAMDEF(JIT_PARAMSTR)

/* JIT compiler state. */
typedef struct _JitState {
  Fragment cur;

  // Current VM state
  Thread *T;
  const BCIns *pc;
  FuncInfoTable *func;

  // Virtual/Recorder State
  TRef *base;      // current base pointer as pointer into slots
  TRef slot[MAX_SLOTS];   // virtual register contents
  BCReg baseslot;  // current base pointer as offset into slot
  BCReg maxslot;   // size of the current frame
                   // INVARIANT: baseslot + maxslot < MAX_SLOTS
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

  u4 sizekwords;
  Word *kwordsbuf;

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

  Word sizeheap;
  Word sizeheapmap;
  HeapInfo *heapbuf;
  HeapEntry *heapmapbuf;

  BCIns *startpc; // Address where recording was started.
  const Word *startbase;

  FoldState fold;
  IRRef1 chain[IR__MAX];

  // Code cache
  Fragment **fragment;
  u4 nfragments;       // number of entries used
  u4 sizefragment;     // total size of table, power of 2

  MCode *exitstubgroup[LC_MAX_EXITSTUBGR];  /* Exit stub group addresses. */

  int mcprot;		/* Protection of current mcode area. */
  MCode *mcarea;	/* Base of current mcode area. */
  MCode *mctop;		/* Top of current mcode area. */
  MCode *mcbot;		/* Bottom of current mcode area. */
  size_t szmcarea;	/* Size of current mcode area. */
  size_t szallmcarea;	/* Total size of all allocated mcode areas. */

  uint32_t prngstate;	/* PRNG state. */

  int32_t param[JIT_P__MAX];  /* JIT engine parameters. */
} JitState;


/* Trivial PRNG e.g. used for penalty randomization. */
static LC_AINLINE uint32_t LC_PRNG_BITS(JitState *J, int bits)
{
  /* Yes, this LCG is very weak, but that doesn't matter for our use case. */
  J->prngstate = J->prngstate * 1103515245 + 12345;
  return J->prngstate >> (32-bits);
}

typedef struct _FragmentEntry {
  u2 unused;
  u2 chain;
  const BCIns *pc;
  Fragment *code;
} FragmentEnty;

typedef enum {
  REC_ABORT = 0,  // Recording has been aborted
  REC_CONT  = 1,  // Continue recording
  REC_LOOP  = 2,  // Loop detected, continue at trace in higher bits
  REC_DONE  = 3,  // Recording finished but not with a loop.
  REC_MASK  = 0xff
} RecordResult;

INLINE_HEADER FragmentId getFragmentId(RecordResult r) { return (u4)r >> 8; }

void initJitState(JitState *J, const Opts* opts);
LC_FASTCALL void startRecording(JitState *J, BCIns *, Thread *, Word *base);
void recordSetup(JitState *J, Thread *T);
FragmentId finishRecording(JitState *J);
LC_FASTCALL TRef emitIR(JitState *J);
LC_FASTCALL TRef emitLoadSlot(JitState *J, i4 slot);
LC_FASTCALL TRef emitKWord(JitState *J, Word w, LitType lt);
RecordResult recordIns(JitState *J);

LC_FASTCALL TRef optFold(JitState *J);
LC_FASTCALL TRef optCSE(JitState *);
LC_FASTCALL void optUnrollLoop(JitState *J);
LC_FASTCALL void optDeadCodeElim(JitState *J);
LC_FASTCALL void optDeadAssignElim(JitState *J);
LC_FASTCALL TRef optForward(JitState *J);
LC_FASTCALL void compactPhis(JitState *J);

void growIRBufferTop(JitState *J);

int irEngine(Capability *cap, Fragment *F);

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
  return optFold(J);
}

INLINE_HEADER void traceError(JitState *J, int n)
{
  exit(n);
}

LC_FASTCALL IRRef findPhi_aux(JitState *J, IRRef ref);

// Find the corresponding PHI node for the given target reference.
//
// Returns a non-zero result iff [ref] is shadowed by a PHI node.
// The result is a reference to the PHI node itself.
INLINE_HEADER IRRef
findPhi(JitState *J, IRRef ref)
{
  if (ref < REF_BIAS || ref >= J->cur.nloop || !irt_getphi(J->cur.ir[ref].t))
    return 0;
  
  return findPhi_aux(J, ref);
}

// Find the corresponding twin of a referenced involved in a PHI node.
//
// Returns a non-zero result iff [ref] is a PHI node.  The returned
// reference is the PHI twin (i.e., the second argument to the PHI
// node).
INLINE_HEADER IRRef
findPhiTwin(JitState *J, IRRef ref)
{
  IRRef ref2 = findPhi(J, ref);
  return ref != 0 ? J->cur.ir[ref2].op2 : 0;
}


#endif
