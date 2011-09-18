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
#include "Thread.h"
#if LC_HAS_ASM_BACKEND
#include "AsmTarget.h"
#endif

#include <stdlib.h>
#include <stdio.h>

#if LC_HAS_JIT

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
  J->sizesnapmap = needed;
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
  IF_DBG_LVL(1, fprintf(stderr, "Created snapshot:\n  ");
             printSnapshot(J, snap, J->cur.snapmap));
}

void
addSnapshot(JitState *J)
{
  DBG_PR("Adding snapshot:\n%s", "");
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
      fprintf(stderr, "[%d]:", j);

    if (nent > 0 && snap_slot(*p) == i) {
      printIRRef(&J->cur, snap_ref(*p));
      ++p;
    } else
      fprintf(stderr, "---- ");
  }
  fprintf(stderr, "pc = %p\n", pc);
}

#if LC_HAS_ASM_BACKEND
/* -- Snapshot restoration ------------------------------------------------ */
/* Initialize a Bloom Filter with all renamed refs.
** There are very few renames (often none), so the filter has
** very few bits set. This makes it suitable for negative filtering.
*/
static BloomFilter snap_renamefilter(Fragment *F, SnapNo lim)
{
  BloomFilter rfilt = 0;
  IRIns *ir;
  for (ir = &F->ir[F->nins-1]; ir->o == IR_RENAME; ir--)
    if (ir->op2 <= lim)
      bloomset(rfilt, ir->op1);
  return rfilt;
}

/* Process matching renames to find the original RegSP. */
static RegSP snap_renameref(Fragment *F, SnapNo lim, IRRef ref, RegSP rs)
{
  IRIns *ir;
  for (ir = &F->ir[F->nins-1]; ir->o == IR_RENAME; ir--)
    if (ir->op1 == ref && ir->op2 <= lim)
      rs = ir->prev;
  return rs;
}

void restoreSnapshot(SnapNo snapno, void *exptr) {
  ExitState *ex = (ExitState *)exptr;
  Thread *T   = ex->T;
  Fragment *F = ex->F;
  SnapShot *snap = &F->snap[snapno];
  u1 n, nent = snap->nent;
  SnapEntry *smap = &F->snapmap[snap->mapofs];
  BloomFilter rfilt = snap_renamefilter(F, snapno);

  /* Recall the base pointer we had on entry */
  Word *base = (Word *)ex->gpr[RID_BASE];

  /* Fill stack slots with data from the registers and spill slots. */
  for (n = 0; n < nent; n++) {
    SnapEntry se = smap[n];
    BCReg s    = snap_slot(se);
    IRRef ref  = snap_ref(se);
    Word *bval = &base[s];
    IRIns *ir  = &F->ir[ref];

    if(irref_islit(ref)) { /* restore constant ref */
      switch(ir->o) {
        case IR_KINT:
          *bval = ir->i;
          break;
        case IR_KWORD:
          *bval = F->kwords[ir->u];
          break;
        case IR_KBASEO:
          //TODO: why is KBASEO against base[0], but other offsets are
          // against base[-1]??
          *bval = (Word)&base[ir->i + 1];
          break;
        default:
          LC_ASSERT(0 && "Unexpected constant ref");
      }
    }
    else { /* non-const reference */
      RegSP rs = ir->prev;

      /* check for rename */
      if (LC_UNLIKELY(bloomtest(rfilt, ref))){
        rs = snap_renameref(F, snapno, ref, rs);
      }

      /* restore from spill slot */
      if(ra_hasspill(regsp_spill(rs))) {
        *bval = ex->spill[regsp_spill(rs)];
      }
      /* or restore from register */
      else {
        Reg r = regsp_reg(rs);
	LC_ASSERT(ra_hasreg(r));
        *bval = ex->gpr[r];
      }
    }

    IF_DBG_LVL(1,
               fprintf(stderr, "base[%d] = ", s - 1);
               printSlot(stderr, base + s);
               fprintf(stderr, "\n")
    );
  }

  /* Restore pc, base, and top pointers for the thread */
  DBG_PR("Base slot: %d\n", smap[nent+1]);
  T->pc   = (BCIns *)F->startpc + (int)smap[nent];
  T->base = base + smap[nent+1];
  T->top  = base + snap->nslots;
}
#endif	/* LC_HAS_ASM_BACKEND */

#undef IR

#endif  /* LC_HAS_JIT */
