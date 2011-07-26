#include "Thread.h"
#include "StorageManager.h"
#include "PrintClosure.h"
#include "InfoTables.h"

/*

Where can GC happen?

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

// Return a pointer to the bitmask associated with the current PC.
// May return NULL.  In that case, the bitmap is to be extracted from
// the stack.
INLINE_HEADER const u2 *
getPointerMask(BCIns *next_pc)
{
  BCIns *p0 = &next_pc[-1];
  u4 offset = (u4)(*p0);
  if (offset != 0)
    return (const u2*)((u1*)p0 + offset);
  else
    return NULL;
}

void evacuate(Closure **p);
void scavengeBlock(BlockDescr *bd);

typedef struct _GCContext {
  BlockDescr *todo;
} GCContext;

GCContext G_gc;

void
evacuateFrame(Word *base, BCIns *pc)
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

  M->gc_inprogress = 1;
  makeCurrent(M, getEmptyBlock(M));

  dumpStorageManagerState();

  G_gc.todo = M->full;
  M->full = NULL;
  M->nfull = 0;
  
  LC_ASSERT(lo <= base);

  

  printf("Garbage collecting...\n");
  printf("stack = %p..%p (+ N)\n", lo, base);
  printf("  0: frame = %p..?\n", base);
  printf("     pc = %p, ", pc);
  printInlineBitmap(pc - 1);

  evacuateFrame(base, pc);

  // const u2 *live_ptr = getPointerMask(T->pc);
  i = 1;
  while (base >= lo) {
    pc = (BCIns*)base[-2];
    hi = &base[-3];
    base = (Word*)base[-3];
    if (base > hi || base < lo) break;
    printf(" %2u: frame = %p..%p (%d)\n", i, base, hi, (int)(hi - base));
    printf("     pc = %p, ", pc);
    ++i;
    printInlineBitmap(pc - 1);
    evacuateFrame(base, pc);
    //    print
  }
  scavengeBlock(M->current);

  exit(1);

  G_storage.gc_inprogress = 0;
}

#define MK_FORWARDING_PTR(info) ((Word)info | 1)
#define IS_FORWARDING_PTR(info) ((Word)info & 1)
#define UN_FORWARDING_PTR(p)    ((Word*)((Word)p & ~1))

#define IS_HEAP(bd) (((bd)->flags & BF_CONTENTS_MASK) == BF_CLOSURES)

#define TICK_GC_WORDS_COPIED(sz)  do {} while (0)

#define CONSTR_SIZE(info)       ((info)->size + 1)
#define THUNK_SIZE(info)       ((info)->size + 1)

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
  
  TICK_GC_WORDS_COPIED(size);

  from = (Word*)src;
  to[0] = (Word)info;
  for (i = 1; i < size; i++) {
    to[i] = from[i];
  }

  src->header.info = (const InfoTable *)MK_FORWARDING_PTR(to);
  *p = (Closure*)to;
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

 loop:
  bd = bDescr(q);
  if (!IS_HEAP(bd)) {
    DBG_LVL(2, "~~ S: Skipping static object: %p\n", q);
    // We don't collect static objects yet.
    return;
  }
  
  info = q->header.info;
  if (IS_FORWARDING_PTR(info)) {
    *p = (Closure*)UN_FORWARDING_PTR(info);
    DBG_LVL(2, "~~ F: FwdPtr found: %p -> %p\n", q, *p);
    return;
  }

  switch (info->type) {
  case CONSTR:
    DBG_LVL(2, "~~ C: Evacuating CONSTR: %p, %d\n", q, CONSTR_SIZE(info));
    printClosure(q);
    copy(p, info, q, CONSTR_SIZE(info));
    return;

    //case FUN:
    //break;

  case THUNK:
    DBG_LVL(2, "~~ T: Evacuating THUNK: %p, %d\n", q, THUNK_SIZE(info));
    printClosure(q);
    copy(p, info, q, THUNK_SIZE(info));
    return;

  case IND:
    DBG_LVL(1, "~~ I: Following IND: %p ", q);
    q = cast(IndClosure*,q)->indirectee;
    DBG_LVL(1, "-> %p\n", q);
    LC_ASSERT(q != NULL);
    *p = q;
    goto loop;

  default:
    DBG_LVL(2, "** Don't yet know how to evacuate closure type: %d\n", info->type);
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

  printf("Scavenging block:  %p-%p\n", bd->start, bd->free);

  p = (Word*)bd->start;

  // We might be evacuating into the same block that we're scavenging.
  // That is `bd->free` might change during the loop, so recheck here.
  while ((char*)p < bd->free) {
    
    info = getInfo(p);
    q = p;

    DBG_LVL(2, "~ Scavenging %p ", p);
    printClosure((Closure*)p);
    switch (info->type) {
    case CONSTR:
      {
        Word *q = p + 1;
        bitmap = info->layout.bitmap;
        int j = 0;
        for (i = info->size; i > 0 && bitmap != 0; i--, bitmap >>= 1) {
          j++;
          if (bitmap & 1) {
            DBG_PR("Evacuating: %p[%d] / %p\n", p, j, q);
            evacuate((Closure**)q);
          }
          q++;
        }
        printClosure((Closure*)p);
        p += info->size + 1;
      }
      break;
    default:
      fprintf(stderr, "Don't know how to scavenge objects of type %d, yet\n",
              info->type);
      exit(2);
    }
  }
}
