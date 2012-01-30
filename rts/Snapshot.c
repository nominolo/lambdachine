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
#include "Stats.h"
#if LC_HAS_ASM_BACKEND
#include "AsmTarget.h"
#include "StorageManager.h"
#include "HeapInfo.h"
#include "AsmTarget_x64.h"
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

/**
 * Creates snapshot entries for all slots between J->minslot and
 * J->baseslot + J->maxslot.
 *
 * @param J the JIT state.
 * @param map entries are added to this array. Assumes that there is
 *     enough space in that array.
 */
static Word
snapshotSlots(JitState *J, SnapEntry *map)
{
  i2 s;
  Word n = 0;

  for (s = J->minslot; s < J->baseslot + J->maxslot; s++) {
    TRef tr = J->slot[s];
    IRRef ref = tref_ref(tr);
    i2 relslot = s - INITIAL_BASE;

    if (ref) {
      SnapEntry sn = SNAP_TR(relslot, tr);
      IRIns *ir = IR(ref);
      if (ir->o == IR_SLOAD && ir->op1 == relslot)
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
  map[1] = (int)J->baseslot - (int)INITIAL_BASE;
  // Note that framesize denotes the maximum size of *our* frame.
  // If we go below INITIAL_BASE it means we are manipulating an existing
  // frame.  We don't need to allocate anything for that.
  J->framesize = MAX(J->framesize,
		     J->baseslot + J->maxslot - 1 - INITIAL_BASE);
}

static void
snapshotStack(JitState *J, SnapShot *snap, Word nsnapmap)
{
  // Maximum possible number of stack slots needed by this snapshot.
  int nslots = (int)J->baseslot + (int)J->maxslot - (int)J->minslot;
  Word nent;
  SnapEntry *p;

  growSnapshotMapBuffer(J, nsnapmap + nslots + 2);
  p = &J->cur.snapmap[nsnapmap];
  nent = snapshotSlots(J, p);
  snapshotFrame(J, p + nent);
  snap->mapofs = (u2)nsnapmap;
  snap->ref = (IRRef1)J->cur.nins;
  snap->nslots = (u1)nslots;
  snap->nent = (u1)nent;
  snap->count = 0;
  snap->minslot = J->minslot - INITIAL_BASE;
  J->cur.nsnapmap = nsnapmap + nent + 2;
  IF_DBG_LVL(1, fprintf(stderr, "Created snapshot:\n  ");
             printSnapshot(&J->cur, snap, J->cur.snapmap));
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
printSnapshot(Fragment *F, SnapShot *snap, SnapEntry *map)
{
  if (snapShotRemoved(snap)) {
    //    DBG_PR("Not printing snapshot %p\n", snap);
    return;
  }

  SnapEntry *p = &map[snap->mapofs];
  int i;
  u4 nent = snap->nent;
  int nslots = snap->nslots;
  int minslot = snap->minslot;
  int baseslot = (int)p[nent + 1];
  const BCIns *pc = F->startpc + (ptrdiff_t)(i4)p[nent];

  for (i = minslot; i < minslot + nslots; i++) {
    int slot_id = i - baseslot;
    if ((slot_id & 3) == 0)
      fprintf(stderr, "[%d]:", slot_id);

    if (nent > 0 && snap_slot(*p) == i) {
      printIRRef(F, snap_ref(*p));
      ++p;
    } else
      fprintf(stderr, "---- ");
  }
  fprintf(stderr, "pc = %p, ref = %d\n", pc, irref_int(snap->ref));
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

static Word
snap_restoreval(Fragment *F, IRRef ref,
                ExitState *ex, Word *base,
                BloomFilter rfilt, SnapNo snapno)
{
  IRIns *ir = &F->ir[ref];

  if(irref_islit(ref)) { /* restore constant ref */
    switch(ir->o) {
    case IR_KINT:
      return ir->i;
    case IR_KWORD:
      return F->kwords[ir->u];
    case IR_KBASEO:
      //TODO: why is KBASEO against base[0], but other offsets are
      // against base[-1]??
      return (Word)&base[ir->i + 1];
    default:
      LC_ASSERT(0 && "Unexpected constant ref");
    }
  }
  else { /* non-const reference */
    if (ir->o == IR_NEW && ir_issunken(ir)) {
      HeapInfo *hpi = getHeapInfo(F, ir);
      int j;
      Closure *cl = allocClosure(wordsof(ClosureHeader) + hpi->nfields);
      LC_ASSERT(irref_islit(ir->op1));
      setInfo(cl, (InfoTable*)snap_restoreval(F, ir->op1, ex, base,
                                              rfilt, snapno));
      for (j = 0; j < hpi->nfields; j++) {
        DBG_LVL(3, "{%d:%d}", j, irref_int(getHeapInfoField(F, hpi, j)));
        cl->payload[j] = snap_restoreval(F, getHeapInfoField(F, hpi, j),
                                         ex, base, rfilt, snapno);
      }
      return (Word)cl;
    } else {

      RegSP rs = ir->prev;

      /* check for rename */
      if (LC_UNLIKELY(bloomtest(rfilt, ref))){
        rs = snap_renameref(F, snapno, ref, rs);
      }

      /* restore from spill slot */
      if(ra_hasspill(regsp_spill(rs))) {
        DBG_LVL(3, "{spilled: %d}", regsp_spill(rs));
        return ex->spill[regsp_spill(rs)];
      }
      /* or restore from register */
      else {
        DBG_LVL(3, "{reg: %s}", ra_regname[regsp_reg(rs)]);
        Reg r = regsp_reg(rs);
        LC_ASSERT(ra_hasreg(r));
        return ex->gpr[r];
      }
    }
  }
}

void restoreSnapshot(SnapNo snapno, void *exptr) {
  ExitState *ex = (ExitState *)exptr;
  Thread *T   = ex->T;
  Fragment *F = ex->F;
  SnapShot *snap = &F->snap[snapno];
  u1 n, nent = snap->nent;
  SnapEntry *smap = &F->snapmap[snap->mapofs];
  BloomFilter rfilt = snap_renamefilter(F, snapno);
  bool need_gc = false;

  DBG_LVL(1, "Restoring Snapshot: %d\n", snapno);

  recordEvent(EV_EXIT, snap->nent);

  /* A SAVE snapshot exit is only ever triggered if we have a stack
     overflow. */
  if (F->ir[snap->ref].o == IR_SAVE) {
    sayonara("Stack overflow\n");
  }

  G_storage.gc_inhibited = 1;
  G_storage.hp = (Word*)ex->gpr[RID_HP];
  G_storage.limit = ex->hplim;

  /* If (Hp > HpLim) then we had a heap overflow, look at the snapshot
     to figure out how much memory we actually wanted to allocate. */

  if (LC_UNLIKELY(G_storage.hp > G_storage.limit)) {
    DBG_PR("Trace exited (#%d) due to heap overflow check.\n",
           snapno);
    IRIns *ir = &F->ir[snap->ref];
    LC_ASSERT(ir->o == IR_HEAPCHK);
    G_storage.hp -= ir->u;      /* Reset Hp to pre-bumped value. */

    markCurrentBlockFull(&G_storage);
    makeCurrent(&G_storage, getEmptyBlock(&G_storage));

    if (G_storage.nfull >= G_storage.nextgc) {
      need_gc = true;
    }
  }

  DBG_LVL(1, "POST trace: Hp = %p, HpLim = %p\n",
          G_storage.hp, G_storage.limit);

  /* Recall the base pointer we had on entry */
  Word *base = (Word *)ex->gpr[RID_BASE];

  /* Fill stack slots with data from the registers and spill slots. */
  for (n = 0; n < nent; n++) {
    SnapEntry se = smap[n];
    i2 s    = snap_slot(se);
    IRRef ref  = snap_ref(se);
    Word *bval = &base[s];

    *bval = snap_restoreval(F, ref, ex, base, rfilt, snapno);

    IF_DBG_LVL(1,
               fprintf(stderr, "base[%d] = ", s - 1);
               printSlot(stderr, base + s);
               fprintf(stderr, "\n")
    );
  }

  G_storage.gc_inhibited = 0;
  if (need_gc) {
    /* Force a GC next time an allocation is triggered. */
    G_storage.limit = G_storage.hp;
  }

  /* Restore pc, base, and top pointers for the thread */
  DBG_PR("Base slot: %d\n", smap[nent+1]);
  T->pc   = (BCIns *)F->startpc + (int)smap[nent];
  T->base = base + smap[nent+1];
  T->top  = base + snap->nslots;

  // exit(0xac);
}
#endif	/* LC_HAS_ASM_BACKEND */

#undef IR

#endif  /* LC_HAS_JIT */
