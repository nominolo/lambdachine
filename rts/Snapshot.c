//
// Handling of snapshots.
//
// Snapshots are used to restore the interpreter stack when a guard
// fails.  During trace recording a snapshot contains a mapping from
// each modified virtual register to the IRIns that computes its
// value.  The register allocator later turns this into a register
// name or stack slot index.
//
#include "Snapshot.h"
#include "PrintIR.h"

#include <stdlib.h>
#include <stdio.h>

// -- Snapshot buffer management -------------------------------------

// Maximum number of snapshots per trace.
#define JIT_MAXSNAP      100

// -- Convenience macros.  Undefined at end of file. -----------------

// Pointer to referenced IR.
#define IR(ref)     (&J->cur.ir[(ref)])


// Grow snapshot buffer of current trace.
void
growSnapshotBuffer_(JitState *J, Word needed)
{
  Word maxsnap = JIT_MAXSNAP;
  if (needed > maxsnap)
    traceError(J, 213);

  J->snapbuf = realloc(J->snapbuf, maxsnap * sizeof(SnapShot));
  J->sizesnap = needed;
  J->cur.snap = J->snapbuf;
}

void
growSnapshotMapBuffer_(JitState *J, Word needed)
{
  if (needed < 2 * J->sizesnapmap)
    needed = 2 * J->sizesnapmap;
  else if (needed < 64)
    needed = 64;

  J->snapmapbuf = realloc(J->snapmapbuf, needed * sizeof(SnapEntry));
  J->cur.snapmap = J->snapmapbuf;
  J->sizesnap = needed;
}

// -- Snapshot creation ----------------------------------------------

static Word
snapshotSlots(JitState *J, SnapEntry *map, BCReg nslots)
{
  BCReg s;
  Word n = 0;

  for (s = 0; s < nslots; s++) {
    TRef tr = J->slot[s];
    IRRef ref = tref_ref(tr);

    if (ref) {
      SnapEntry sn = SNAP_TR(s, tr);
      IRIns *ir = IR(ref);
      if (ir->o == IR_SLOAD && ir->op1 == s)
	continue;  // Slot has only been read, not modified
      // TODO: There may be cases where we don't need to save the
      // slot.
      map[n++] = sn;
    }
  }
  return n;
}

static void
snapshotFrame(JitState *J, SnapEntry *map)
{
  i8 d = J->pc - J->startpc;
  LC_ASSERT(in_range_i4(d));
  //printf("d = %d, base = %d\n", (i4)d, J->baseslot);
  map[0] = (SnapEntry)d;
  map[1] = J->baseslot;
}

static void
snapshotStack(JitState *J, SnapShot *snap, Word nsnapmap)
{
  BCReg nslots = J->baseslot + J->maxslot;
  Word nent;
  SnapEntry *p;

  growSnapshotMapBuffer(J, nsnapmap + nslots + 2);
  p = &J->cur.snapmap[nsnapmap];
  nent = snapshotSlots(J, p, nslots);
  snapshotFrame(J, p + nent);
  snap->mapofs = (u2)nsnapmap;
  snap->ref = (IRRef1)J->cur.nins;
  snap->nslots = (u1)nslots;
  snap->nent = (u1)nent;
  snap->count = 0;
  J->cur.nsnapmap = nsnapmap + nent + 2;
  printf("Created snapshot:\n  ");
  printSnapshot(J, snap, J->cur.snapmap);
}

void
addSnapshot(JitState *J)
{
  printf("Adding snapshot:\n");
  Word nsnap = J->cur.nsnap;
  Word nsnapmap = J->cur.nsnapmap;
  // Merge snapshots if:
  //   - there are no instructions in between, or
  //   - TODO: requested and no guard inbetween
  if (nsnap > 0 &&
      J->cur.snap[nsnap - 1].ref == J->cur.nins) {
    nsnapmap = J->cur.snap[--nsnap].mapofs;
  } else {
    growSnapshotBuffer(J, nsnap + 1);
    J->cur.nsnap = (u2)(nsnap + 1);
  }
  J->mergesnap = 0;
  snapshotStack(J, &J->cur.snap[nsnap], nsnapmap);
}

void
printSnapshot(JitState *J, SnapShot *snap, SnapEntry *map)
{
  SnapEntry *p = &map[snap->mapofs];
  int i;
  u4 nent = snap->nent;
  u4 nslots = snap->nslots;
  u4 baseslot = (u4)p[nent + 1];
  const BCIns *pc = J->cur.startpc + (ptrdiff_t)(i4)p[nent];

  for (i = 0; i < nslots; i++) {
    int j = i - baseslot;
    if ((j & 3) == 0)
      printf("[%d]:", j);

    if (snap_slot(*p) == i) {
      printIRRef(J, snap_ref(*p));
      ++p;
    } else
      printf("---- ");
  }
  printf("pc = %p\n", pc);
}

#undef IR
