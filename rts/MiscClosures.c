/*
**
** Defines various closures used by the runtime system.
**
*/

#include "Bytecode.h"
#include "InfoTables.h"
#include "MiscClosures.h"
#include "PrintClosure.h"
#include "StorageManager.h"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define DEF_CODE(code_, framesize_, arity_) \
  { .lits = NULL, .littypes = NULL, .sizelits = 0,   \
    .code = (code_), .sizecode = sizeof(code_) / sizeof(BCIns), \
    .framesize = (framesize_), .arity = (arity_) }


/*

Stack Stop Frame
----------------

We put this closure at the bottom of every thread's stack.  When
control flow reaches this frame, the thread stops.

TODO: We currently stop the VM if a thread halts.  At some point
this should be changed to tell the scheduler to kill the thread.

*/

static BCIns stop_code_insts[] =
  { BCINS_AD(BC_EVAL, 0, 0),   // eval r0 ; no lives
    8,
    BCINS_AD(BC__MAX, 0, 0),    // stop
    0x00010001  // r0 is live and a pointer
  };

/* LcInfoTable stg_STOP_info = */
/*   DEF_INFO(FUN, 0, 0, stop_code_insts, 1, 1, "stg_STOP"); */

ThunkInfoTable stg_STOP_info =
  { .i = DEF_INFO_TABLE(FUN, 0, 0, 0),
    .name = "stg_STOP",
    .code = DEF_CODE(stop_code_insts, 1, 1)
  };

Closure stg_STOP_closure = DEF_CLOSURE(&stg_STOP_info, {});

/*

Update frames
-------------

*/


// Both variables are initialized fully by initMiscClosures().
BCIns *stg_UPD_return_pc = NULL;
Closure stg_UPD_closure = DEF_CLOSURE(NULL, {});

/*

Indirections
------------

We don't need a (static) closure for indirections, because
indirections are only created by overwriting an existing closure's
info table.

*/

// Indirections are currently followed by EVAL code, so we don't need
// any bytecode for it.
// 
//static BCIns ind_code_insts[] =
//  { BCINS_AD(BC_LOADFV, 0, 0), // r0 = Node[0]
//    BCINS_AD(BC_RET1, 0, 0)    // return r0
//  };

ConInfoTable stg_IND_info = {
  .i = DEF_INFO_TABLE(IND, 0, 1, 0),
  .name = "stg_IND"
};

/*

Blackhole
---------

*/

static BCIns blackhole_code_insts[] =
  // TODO: Add an error message.
  { BCINS_AD(BC__MAX, 1, 0) };

ThunkInfoTable stg_BLACKHOLE_info = {
  .i = DEF_INFO_TABLE(BLACKHOLE, 0, 0, 0),
  .name = "stg_BLACKHOLE",
  .code = DEF_CODE(blackhole_code_insts, 1, 0)
};

Closure stg_BLACKHOLE_closure = DEF_CLOSURE(&stg_BLACKHOLE_info, {});


// PAPs
// ----

PapInfoTable stg_PAP_info = {
  .i = DEF_INFO_TABLE(PAP, 0, 0, 0),  // special layout
  .name = "stg_PAP"
};

/*

Int
---

*/

ConInfoTable stg_Izh_con_info = {
  .i = DEF_INFO_TABLE(CONSTR, 0, 0, 1),
  .name = "I#"
};

IntClosure the_smallInt[256];

// Turn a bitmap into a string.  E.g., 0x19 becomes "ppnnp".
// Lowest bit represents the pointerhood of the leftmost item.
// Automatically zero-terminates the string.
// Returns the number of characters printed (excluding terminating
// zero).
int
formatBitmask(char *out, int nargs, u4 pointer_mask)
{
  int i;

  for (i = 0; i < nargs; i++) {
    *out = (pointer_mask & 1) ? 'p' : 'n';
    ++out;
    pointer_mask >>= 1;
  }
  *out = '\0';

  return nargs;
}

int
bitmapSize(u4 bitmap)
{
  if (bitmap < (1u << 15))
    return 1;
  if (bitmap < (1u << 30))
    return 2;
  else
    return 3;
}

#define BITMASK_MASK   ((1u << 15) - 1)
#define BITMASK_CONT   (1u << 15)

// Write bitmask to dest.  Returns number of u2's written.
int
encodeBitmask(u2 *dest, u4 bitmap)
{
  int i, s;
  u2 m;
  s = bitmapSize(bitmap);
  for (i = 0; i < s; i++) {
    m = (u2)(bitmap & BITMASK_MASK);
    if (i < s - 1)
      m |= BITMASK_CONT;
    *dest = m;
    dest++;
    bitmap >>= 15;
  }
  return s;
}


/*

Application continuations
-------------------------

Shape of these closures.  `N` is the number of arguments to apply.

TODO: Currently assumes all arguments are pointers.  GHC distinguishes
between ap_pp, ap_vp, ap_vv, etc.  The exact argument types are:

    n   -- non-ptr
    p   -- ptr
    v   -- void
    f   -- float
    d   -- double
    l   -- long (64-bit)

However, only a few combinations are very common (and the rest can be
achieved by multiple applications).  In particular, GHC only generates
code for the following combinations (see <ghc>/utils/genapply/GenApply.hs):

    v
    f
    d
    l
    n
    p
    pv
    pp
    ppv
    ppp
    pppv
    pppp
    ppppp
    pppppp

For now, though, we treat everything as a pointer (until we have a
garbage collector).


  code:
    1. a bitmask where r0...r(N-1) are live
    2. MOV_RES rN
    3. CALLT rN, N
    framesize = N + 1
    arity = N  (though, unused)

  info table:
    ptr/nptr:  currently we have ptr = N, nptr = 0.  (see TODO above)
    type: FUN
    tag:  0


*/

// -------------------------------------------------------------------

// #define MAX_APK_ARITY  8

// arity  pointer mask           indices              formula
//   1    n, p                2  0,1                  2-2..4-3
//   2    nn, np, pn, pp      6  2,3,4,5              4-2..8-3
//   3    nnn, nnp, ...      14  6,7,8,9,10,11,12,13  8-2..16-3
//   4    ...                30  14,15,..,29          16-2..32-3
#define APK_INDEX(nargs, mask)  ((1u << (nargs)) - 2 + mask)
#define MAX_APK_INDEX  APK_INDEX(MAX_APK_ARITY, (1u << MAX_APK_ARITY) - 1)

typedef struct {
  Closure   *clos;
  BCIns     *return_pc;
} APKInfo;

APKInfo apk_info[MAX_APK_INDEX + 1];
InfoTable *ap_itbls[MAX_APK_INDEX + 1];

void
initUpdateClosure(void)
{
  FuncInfoTable *info = allocInfoTable(wordsof(FuncInfoTable));
  info->i.type = UPDATE_FRAME;
  info->i.size = 2;
  info->i.tagOrBitmap = 0;
  info->i.layout.bitmap = 0;
  info->name = "stg_UPD";
  info->code.framesize = 2;
  info->code.arity = 1;  // kind of bogus, since we never call it
  info->code.sizecode = 4;
  info->code.sizelits = 0;
  info->code.sizebitmaps = 2;
  info->code.lits = NULL;
  info->code.littypes = NULL;
  info->code.code = xmalloc(info->code.sizecode * sizeof(BCIns) +
			    info->code.sizebitmaps * sizeof(u2));
  BCIns *code = info->code.code;
  u2 *bitmasks = cast(u2*, code + info->code.sizecode);
  
  // The EVAL instructions isn't actually executed.  It is only there to
  // attach the bitmap to describe the stack frame.

  code[0] = BCINS_AD(BC_EVAL, 0, 0);  // never executed
  code[1] = cast(BCIns, byte_offset(&code[1], bitmasks));
  code[2] = BCINS_AD(BC_MOV_RES, 1, 0);
  code[3] = BCINS_AD(BC_UPDATE, 0, 1);
  bitmasks += encodeBitmask(bitmasks, 1); // reg 0 is a pointer
  bitmasks += encodeBitmask(bitmasks, 1); // reg 0 is live

  stg_UPD_return_pc = &code[2];
  setInfo(&stg_UPD_closure, (InfoTable*)info);
}

void
initMiscClosures(void)
{
  int i;

  initUpdateClosure();

  for (i = 0; i < sizeof(apk_info) / sizeof(APKInfo); i++) {
    apk_info[i].clos = NULL;
    apk_info[i].return_pc = NULL;
  }
  for (i = 0; i < sizeof(ap_itbls) / sizeof(InfoTable*); i++) {
    ap_itbls[i] = NULL;
  }
}



//
// Code for an APK closure of length N
//
//   IFUNC <N+1>   ; never executed
//   EVAL <N>      ; never executed
//   <bitmask>  ; r0..r<N-1> are live, pointers variable
//   MOV_RES <N>   ;
//   CALLT <N>, r0..r<N-1>
//

void
getApContClosure(Closure **res_clos, BCIns **res_pc,
                 int nargs, u4 pointer_mask)
{
  LC_ASSERT(nargs >= 1 && nargs <= MAX_APK_ARITY);
  LC_ASSERT(pointer_mask < (1u << nargs));

  u4 idx = APK_INDEX(nargs, pointer_mask);
  char buf[100];
  char *p;

  if (LC_UNLIKELY(apk_info[idx].clos == NULL)) {
    
    ApContInfoTable *info = allocInfoTable(wordsof(ApContInfoTable));
    info->i.type = AP_CONT;
    info->i.size = nargs;
    info->i.tagOrBitmap = pointer_mask; // not sure what this is good for
    info->i.layout.bitmap = pointer_mask;

    u4 livemask = (1u << nargs) - 1;
    
    p = buf;
    p += sprintf(p, "stg_ApK%d_", nargs);
    p += formatBitmask(p, nargs, pointer_mask);
    p += sprintf(p, "_info");
    info->name = memmove(allocString(p - buf), buf, p - buf + 1);

    info->code.framesize = nargs + 1;
    info->code.arity = nargs;
    info->code.sizecode = 5; // XXX
    info->code.sizelits = 0;
    info->code.sizebitmaps = bitmapSize(pointer_mask) + bitmapSize(livemask);

    info->code.lits = NULL;
    info->code.littypes = NULL;
    info->code.code = xmalloc(info->code.sizecode * sizeof(BCIns) +
                              info->code.sizebitmaps * sizeof(u2));

    BCIns *code = info->code.code;
    u2 *bitmasks = cast(u2 *, code + info->code.sizecode);

    code[0] = BCINS_AD(BC_FUNC, nargs + 1, 0);
    code[1] = BCINS_AD(BC_EVAL, nargs, 0);
    code[2] = cast(BCIns, byte_offset(&code[2], bitmasks));
    code[3] = BCINS_AD(BC_MOV_RES, nargs, 0);
    code[4] = BCINS_ABC(BC_CALLT, nargs, pointer_mask, nargs);
    bitmasks += encodeBitmask(bitmasks, pointer_mask);
    bitmasks += encodeBitmask(bitmasks, livemask);

    Closure *cl = allocStaticClosure(wordsof(ClosureHeader));
    setInfo(cl, (InfoTable*)info);

    apk_info[idx].clos = cl;
    apk_info[idx].return_pc = &code[3];
  }
  
  *res_clos = apk_info[idx].clos;
  *res_pc = apk_info[idx].return_pc;  
}

InfoTable *
getApInfoTable(int nargs, u4 pointer_mask)
{
  LC_ASSERT(nargs >= 1 && nargs <= MAX_AP_ARGS);
  LC_ASSERT(pointer_mask < (1u << nargs));

  u4 idx = APK_INDEX(nargs, pointer_mask);
  char buf[100];

  if (LC_UNLIKELY(ap_itbls[idx] == NULL)) {
    ApInfoTable *info = allocInfoTable(wordsof(ApInfoTable));
    info->i.type = THUNK;
    info->i.size = 1 + nargs;
    // pointer mask is for the arguments only
    // the function itself is always a pointer.
    info->i.tagOrBitmap = (pointer_mask << 1) | 1u;
    info->i.layout.bitmap = (pointer_mask << 1) | 1u;
    
    char *p = buf;
    p += sprintf(p, "stg_Ap%d_", nargs);
    p += formatBitmask(p, nargs, pointer_mask);
    p += sprintf(p, "_info");
    info->name = memmove(allocString(p - buf), buf, p - buf + 1);

    info->code.framesize = nargs + 1;
    info->code.arity = nargs;
    info->code.sizecode = nargs + 6;  // TODO
    info->code.sizelits = 0;
    info->code.sizebitmaps = bitmapSize(0) + bitmapSize(0);
    info->code.lits = NULL;
    info->code.littypes = NULL;
    info->code.code = xmalloc(info->code.sizecode * sizeof(BCIns) +
                              info->code.sizebitmaps * sizeof(u2));

    BCIns *code = info->code.code;
    u2 *bitmasks = cast(u2 *, code + info->code.sizecode);

    code[0] = BCINS_AD(BC_FUNC, nargs + 1, 0);
    code[1] = BCINS_AD(BC_LOADFV, nargs, 1);  // load function closure
    code[2] = BCINS_AD(BC_EVAL, nargs, 0);
    code[3] = cast(BCIns, byte_offset(&code[3], bitmasks));
    bitmasks += encodeBitmask(bitmasks, 0);  // no live pointers
    bitmasks += encodeBitmask(bitmasks, 0);   // no live variables
    code[4] = BCINS_AD(BC_MOV_RES, nargs, 0);
    int i;
    for (i = 0; i < nargs; i++) {
      code[5 + i] = BCINS_AD(BC_LOADFV, i, i + 2);
    }
    code[5 + nargs] = BCINS_ABC(BC_CALLT, nargs, pointer_mask, nargs);
    
    ap_itbls[idx] = cast(InfoTable*, info);
  }

  return ap_itbls[idx];
}
 
void
dumpApClosures(void)
{
  int i, n;
  fprintf(stderr, "************************************************************\n");
  fprintf(stderr, "Closures generated:\n  ApCont:");
  n = 0;
  for (i = 0; i <= MAX_APK_INDEX; i++) {
    if (apk_info[i].clos != NULL) {
      ++n;
      if (n % 4 == 0) fputc('\n', stderr);
      fprintf(stderr, " %s", cast(ApContInfoTable*,getInfo(apk_info[i].clos))->name);
    }
  }
  fprintf(stderr, "\n  Ap: ");
  n = 0;
  for (i = 1; i <= MAX_APK_INDEX; i++) {
    if (ap_itbls[i] != NULL) {
      ++n;
      if (n % 4 == 0) fputc('\n', stderr);
      fprintf(stderr, " %s", cast(ApInfoTable*,ap_itbls[i])->name);
    }
  }
  fprintf(stderr, "\n");
 }


