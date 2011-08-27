#include "Thread.h"
#include "StorageManager.h"
#include "PrintClosure.h"
#include "InfoTables.h"

#include <string.h>

/*

How the GC works
----------------

We use a simple Cheney-style copying GC.  The two core operations are:

  - Evacuate.  Copy a closure from from-space into to-space.  The info
    table of the old object is overwritten with a forward reference, a
    (tagged) pointer to the new closure.  Evacuating an object that
    already resides in to-space is a no-op.

  - Scavenge.  Traverse the pointer fields of an object and evacuate
    each one.

A GC therefore consists of two phases:

 1. Scan (scavenge) all roots.  Roots currently are all updated CAFs
    and the stack.

 2. Scavenge all objects that were evacuated by the root scan.  This
    may in turn cause more objects to be evacuated which are then
    scavenged and may cause more objects to be evacuated, etc.  This
    continues until all objects have been scavenged.

The heap is allocated in blocks so after scavenged the roots, we
scavenge whole blocks at a time.


Where can GC happen?
--------------------

 - At each allocation site.

 - At each call or eval site, because a stack overflow might happen.

 - [anywhere else?]


Finding the roots
-----------------

The roots are the stacks of all threads.  To find the roots we need to
know which parts of the stack contain pointers.  We can assume that
T->base and T->top are up to date.  So at an allocation site the stack
looks as follows:

    T->base --> +-------------+
                |     r0      |
                +-------------+
                |             |
                :             :
                |    rN       |
                +-------------+

After each ALLOC* instruction there is a bitmask which describes which
fields of the current stack frame are (live) pointers.  These need to
be traversed during a GC.

We also need to traverse the other stack frames.  The metadata of a
stack frame looks as follows:

                :             :
                |             |
                +-------------+
                |  prev_base  |
                +-------------+                  <bitmap>
                |      *---------> return_addr:  <some-bytecode>
                +-------------+
                |    Node     |
    T->base --> +-------------+

We arrange that each possible return address is preceded by a bitmap
describing the (live) pointer variables after the CALL or EVAL.  This
bitmap describes the layout of the stack frame pointed to by
prev_base.  Additionally, Node is always a pointer, so it needs to be
traversed as well.

There is one special case for tailcalls.  Any function may potentially
be the target of a tailcall.  The only valid local variables will be
the arguments so we must find out which arguments are pointers.  There
are two options:

 1. Put a bitmask after the CALLT instruction.

 2. Store a bitmask in the called function.

Option (1) has the advantage of only specifying the bitmask where
needed (tailcalls), but if the target function does not use an
argument then it will be kept alive unnecessarily.  Option (2) fixes
this drawback.  This information is only needed when encountering a
stack overflow after a tailcall.  We expect this to be a rare case, so
we go with option (1) and accept its small inaccuracy.

*/


/*

Stack Layout
------------

State after startup:

 T->stack:   +-------------+
             |      *------------> stg_STOP_closure
             +-------------+
 T->base:    |    NULL     | = r0
             +-------------+

Code for stg_STOP_closure:

  EVAL r0
  STOP

State after startThread(T, clos)

 T->stack:   +-------------+
             |      *------------> stg_STOP_closure
             +-------------+
 T->base:    |    clos     | = r0
             +-------------+

If clos was not in HNF:

 T->stack:   +-------------+
             |      *------------> stg_STOP_closure:
             +-------------+             EVAL r0
 T->base:    |    clos     | = r0        <bitmask>
             +-------------+         .-> STOP
             |  T->stack   |         |
             +-------------+         |
             |      *----------------'
             +-------------+       +-----+----------+
             | clos *------------->|  *  |  payload |
             +-------------+       +--|--+----------+
             |             |          v
             :             :        info table
 */


void evacuate(Closure **p);
void scavengeBlock(BlockDescr *bd);

typedef struct _GCContext {
  BlockDescr *old_full;         /* Blocks that were full at the
                                   beginning of GC. */
  BlockDescr *scav_todo;        /* Worker queue for scavenge. */
  BlockDescr *scav_done;        /* Output queue for scavenge. */
} GCContext;

GCContext G_gc;

void
scavengeFrame(Word *base, BCIns *pc)
{
  u2 bitmap;
  const u2 *bitmaps;
  u4 i;

  evacuate((Closure**)&base[-1]);  // Node

  bitmaps = getPointerMask(pc);
  if (bitmaps == NULL) {
    // If the bitmask 
    fprintf(stderr, "TODO: 0 bitmasks\n");
    exit(3);
  }
  do {
    bitmap = *bitmaps;
    bitmaps++;
    for (i = 0; i < 15 && bitmap != 0; i++, bitmap >>= 1) {
      if (bitmap & 1) {
        evacuate((Closure**)&base[i]);
      }
    }
  } while ((bitmap & 1) != 0);
}

void
scavengeStack(Word *base, Word *top, BCIns *pc)
{
  int i = 0;
  Word *frame_top = top;  // just guessing

  DBG_PR("\n--- Scavenging Stack -----------\n");

  while (base) {
    DBG_PR(" %2u: frame = %p..%p (%d)\n"
           "     pc = %p, ",
           i, base, frame_top, (int)(frame_top - base),
           pc);
    IF_DBG(printInlineBitmap(stderr, pc - 1));

    scavengeFrame(base, pc);
    
    pc = (BCIns*)base[-2];
    frame_top = (Word*)&base[-3];
    base = (Word*)base[-3];
    ++i;
  }
}

void
scavengeStaticRoots(Closure *r)
{
  DBG_PR("\n--- Traversing Static Roots -----------\n");
  while (r) {
    evacuate((Closure**)&r->payload[0]);
    r = (Closure*)r->payload[1];
  }
}

void
performGC(Capability *cap)
{
  Thread *T = cap->T;
  Word *lo = T->stack;
  Word *hi;
  Word *base = T->base;
  BCIns *pc = T->pc;
  u4 i;
  StorageManagerState *M = &G_storage;
  u2 bitmap, *bitmaps;

  LC_ASSERT(M->current == NULL);

  M->gc_inprogress = 1;
  makeCurrent(M, getEmptyBlock(M));

  IF_DBG(dumpStorageManagerState());

  G_gc.old_full = M->full; // These will become free at the end of GC
  G_gc.scav_todo = NULL; // Work queue for scavenger
  G_gc.scav_done = NULL; // Surviver blocks end up here
  M->full = NULL;
  M->nfull = 0;
  
  LC_ASSERT(lo <= base);

  DBG_PR("Garbage collecting...\n"
         "stack = %p..%p (+ N)\n", lo, base);

  scavengeStaticRoots(cap->static_objs);
  scavengeStack(base, T->top, pc);

  while (1) {
    // It's important to scavenge full blocks first.  Otherwise we may
    // end up allocating into a block that's already been marked as
    // scavenged.

    while (M->full) {
      BlockDescr *bd = M->full;
      BlockDescr *tmp;
      // Pop from M->full and push onto scav_todo.  This ensures that
      // we scavenge things in the order in which blocks were filled
      // (breadth-first).
      while (bd) {
        tmp = bd->link;
        if (TEST_FLAG(bd->flags, BF_SCAVENGED)) {
          LC_ASSERT(bd->link == NULL);
          bd->link = G_gc.scav_done;
          G_gc.scav_done = bd;
          break;
        }
        bd->link = G_gc.scav_todo;
        G_gc.scav_todo = bd;
        bd = tmp;
      }
      M->full = NULL;
    
      while (G_gc.scav_todo) {
        BlockDescr *bd = G_gc.scav_todo;
        G_gc.scav_todo = bd->link;

        scavengeBlock(bd);
        bd->link = G_gc.scav_done;
        G_gc.scav_done = bd;
      }
    }
    
    // All full blocks scavenged, need to scavenge current block.
    {
      BlockDescr *bd = M->current;
      scavengeBlock(bd);
      
      // Scavenging may have filled more blocks.  In that case we have:
      // 
      //   - M->current is different from bd
      // 
      //   - bd must be full now but it is also linked into the
      //     M->full list so we can't just unlink it here.
      //
      if (M->current == bd) {
        break;
      }
    }
  }

  LC_ASSERT(M->full == NULL);
  M->full = G_gc.scav_done;
  M->nfull = 0;
  // Remove BF_SCAVENGED flag
  {
    BlockDescr *bd;
    for (bd = M->full; bd; bd = bd->link) {
      M->nfull++;
      LC_ASSERT(TEST_FLAG(bd->flags, BF_SCAVENGED));
      CLEAR_FLAG(bd->flags, BF_SCAVENGED);
    }
  }
  CLEAR_FLAG(M->current->flags, BF_SCAVENGED);

  M->nextgc = 2 * M->nfull + 2;
  
  // Push now-dead blocks back onto free list.
  {
    BlockDescr *bd, *tmp;
    bd = G_gc.old_full;
    while (bd) {
      bd->flags = 0;
      bd->free = bd->start;
#ifndef NDEBUG
      // zero out contents to fail quickly in case of bugs
      memset(bd->start, 0, bd->free - bd->start);
#endif
      tmp = bd->link;
      bd->link = M->empty;
      M->empty = bd;
      bd = tmp;
    }
  }

  M->hp = (Word*)M->current->free;
  M->limit = (Word*)BLOCK_END(M->current);

  IF_DBG(dumpStorageManagerState());

  G_storage.gc_inprogress = 0;

  // exit(1);
}

#define MK_FORWARDING_PTR(info) ((Word)info | 1)
#define IS_FORWARDING_PTR(info) ((Word)info & 1)
#define UN_FORWARDING_PTR(p)    ((Word*)((Word)p & ~1))

#define IS_HEAP(bd) (((bd)->flags & BF_CONTENTS_MASK) == BF_CLOSURES)

#define TICK_GC_WORDS_COPIED(sz)  do {} while (0)

#define CONSTR_SIZE(info)       ((info)->size + wordsof(ClosureHeader))
#define THUNK_SIZE(info)       ((info)->size + wordsof(ClosureHeader))
#define FUN_SIZE(info)       ((info)->size + wordsof(ClosureHeader))

Word *
allocForCopy(u4 size)
{
  Word *p = allocClosureDuringGC(size);
  
  return p;
}

static inline void
copy(Closure **p, const InfoTable *info, Closure *src, u4 size)
{
  Word *to, *from;
  u4 i;
  to = allocForCopy(size);
  DBG_LVL(2, " => " COLOURED(COL_GREEN, "%p") " / ", to);
  
  TICK_GC_WORDS_COPIED(size);

  from = (Word*)src;
  to[0] = (Word)info;
  for (i = 1; i < size; i++) {
    to[i] = from[i];
  }

  src->header.info = (const InfoTable *)MK_FORWARDING_PTR(to);
  *p = (Closure*)to;

  {
    //DBG_LVL(2, "%d / ", looksLikeClosure(*p));
    //const InfoTable* info = getInfo((Closure*)to);
    //DBG_LVL(2, "%d / ", looksLikeInfoTable(info));
    DBG_LVL(2, "%d / ", isClosure(*p));
  }

  IF_DBG_LVL(2,printClosure_(stderr, *p, 1));
}

void
evacuate(Closure **p)
{
  Closure *q;
  const InfoTable *info;
  BlockDescr *bd;
  u4 n;
  Word bitmap;
  Word *f;

  q = *p;

  DBG_LVL(2, "~~ Evac: " COLOURED(COL_RED,"%p"), q);

 loop:
  info = q->header.info;

  if (IS_FORWARDING_PTR(info)) {
    *p = (Closure*)UN_FORWARDING_PTR(info);
    DBG_LVL(2, " -F-> " COLOURED(COL_YELLOW, "%p") "\n", *p);
    return;
  }

  bd = bDescr(q);
  if (!IS_HEAP(bd)) {
    // We already traversed thunks.
    DBG_LVL(2, " -S-> " COLOURED(COL_YELLOW, "static object") "\n");
    // We don't collect static objects yet.
    return;
    // Indirections from the static heap may point into the live
    // heap, so we must follow them as roots.
  }
  
  switch (info->type) {
  case CONSTR:
    DBG_LVL(2, " -C(%d)-> ", (int)CONSTR_SIZE(info));
    IF_DBG_LVL(2,printClosure_(stderr, q, 0));
    copy(p, info, q, CONSTR_SIZE(info));
    return;

    //case FUN:
    //break;

  case THUNK:
    DBG_LVL(2, " -T(%d)-> ", (int)THUNK_SIZE(info));
    IF_DBG_LVL(2,printClosure_(stderr, q, 0));
    copy(p, info, q, THUNK_SIZE(info));
    return;

  case FUN:
    DBG_LVL(2, " -F(%d)-> ", (int)FUN_SIZE(info));
    IF_DBG_LVL(2,printClosure_(stderr, q, 0));
    copy(p, info, q, FUN_SIZE(info));
    return;

  case IND:
    q = cast(IndClosure*,q)->indirectee;
    DBG_LVL(2, " -I-> %p", q);
    LC_ASSERT(q != NULL);
    *p = q;
    goto loop;

  default:
    fprintf(stderr, "** Don't yet know how to evacuate closure type: %d\n", info->type);
    exit(2);
    return;
  }
}

void
scavengeBlock(BlockDescr *bd)
{
  Word *p, *q;
  Word *scan;
  Word *lim;
  const InfoTable *info;
  int i;
  u4 bitmap;

  LC_ASSERT(!TEST_FLAG(bd->flags, BF_SCAVENGED));

  DBG_LVL(1, "\n--- Scavenging block:  %p-%p -----------\n", bd->start, bd->free);

  p = (Word*)bd->start;

  // We might be evacuating into the same block that we're scavenging.
  // That is `bd->free` might change during the loop, so recheck here.
  while ((char*)p < bd->free) {
    
    info = getInfo(p);
    q = p;

    DBG_LVL(2, "~ Scav %p: ", p);
    IF_DBG(printClosure_(stderr, (Closure*)p, 1));
    switch (info->type) {
    case CONSTR:
    case THUNK:
    case FUN:
      {
        Word *q = p + 1;
        bitmap = info->layout.bitmap;
        int j = 0;
        for (i = info->size; i > 0 && bitmap != 0; i--, bitmap >>= 1) {
          j++;
          if (bitmap & 1) {
            //DBG_PR("Evacuating: %p[%d] / %p\n", p, j, q);
            evacuate((Closure**)q);
          }
          q++;
        }
        //printClosure((Closure*)p);
        p += info->size + 1;
        if (info->size == 0) p++;
      }
      break;
    default:
      fprintf(stderr, "Don't know how to scavenge objects of type %d, yet\n",
              info->type);
      exit(2);
    }
  }

  SET_FLAG(bd->flags, BF_SCAVENGED);
}
