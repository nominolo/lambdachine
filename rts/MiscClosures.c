/*
**
** Defines various closure used by the runtime system.
**
*/

#include "Bytecode.h"
#include "InfoTables.h"
#include "MiscClosures.h"
#include "PrintClosure.h"
#include "StorageManager.h"

#include <stdlib.h>
#include <stdio.h>

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
    0,
    BCINS_AD(BC__MAX, 0, 0)    // stop
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

/* The EVAL instructions isn't actually executed.  It is only there to
   describe the stack frame.
*/
static BCIns update_code_insts[] =
  { BCINS_AD(BC_EVAL, 0, 0),    // never executed.
    1,                           // reg 0 is alive
    BCINS_AD(BC_MOV_RES, 1, 0),  // r1 = result
    BCINS_AD(BC_UPDATE, 0, 1)
  };

BCIns *stg_UPD_return_pc = &update_code_insts[2];

FuncInfoTable stg_UPD_info = {
  .i = DEF_INFO_TABLE(UPDATE_FRAME, 0, 0, 0),
  .name = "stg_UPD",
  .code = DEF_CODE(update_code_insts, 2, 0)
};

Closure stg_UPD_closure = DEF_CLOSURE(&stg_UPD_info, {});

/*

Indirections
------------

We don't need a (static) closure for indirections, because
indirections are only created by overwriting an existing closure's
info table.

*/

static BCIns ind_code_insts[] =
  { BCINS_AD(BC_LOADFV, 0, 0), // r0 = Node[0]
    BCINS_AD(BC_RET1, 0, 0)    // return r0
  };

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

Closure *apk_closures[MAX_APK_ARITY];
BCIns *apk_return_pcs[MAX_APK_ARITY];
InfoTable *ap_infos[MAX_AP_ARGS - 1];

void initAPClosures()
{
  int i;
  for (i = 0; i < MAX_APK_ARITY; i++) {
    apk_closures[i] = NULL;
    apk_return_pcs[i] = NULL;
  }
  for (i = 0; i < MAX_AP_ARGS - 1; i++) {
    ap_infos[i] = NULL;
  }
}

void
getAPKClosure(Closure **res_clos, BCIns **res_pc, int nargs)
{
  LC_ASSERT(nargs >= 1 && nargs <= MAX_APK_ARITY + 1);

  if (apk_closures[nargs - 1] == NULL) { // TODO: annotate as unlikely
    // Create closure

    FuncInfoTable *info = allocInfoTable(wordsof(FuncInfoTable));
    info->i.type = FUN;
    info->i.tagOrBitmap = 0;
    info->i.layout.payload.ptrs = nargs;  // TODO: see comments above
    info->i.layout.payload.nptrs = 0;
    asprintf(&info->name, "stg_APK%d_info", nargs);
    info->code.framesize = nargs + 1;
    info->code.arity = nargs;
    info->code.sizelits = 0;
    info->code.sizecode = 5;
    info->code.lits = NULL;
    info->code.littypes = NULL;
    info->code.code = xmalloc(info->code.sizecode * sizeof(BCIns));

    BCIns *code = info->code.code;

    // The EVAL at the beginning is never executed.  It is only a place to
    // attach the liveness mask and to make sure the closure code can be
    // printed.
    code[0] = BCINS_AD(BC_FUNC, nargs + 1, 0);
    code[1] = BCINS_AD(BC_EVAL, nargs, 0);
    code[2] = cast(BCIns, (1 << nargs) - 1); // liveness mask
    code[3] = BCINS_AD(BC_MOV_RES, nargs, 0); // rN = result
    code[4] = BCINS_AD(BC_CALLT, nargs, nargs);

    Closure *cl = allocStaticClosure(wordsof(ClosureHeader));  // no payload
    setInfo(cl, (InfoTable*)info);

    //printf("\033[34mCreated closure: %s (%p)\n", info->name, cl);
    //printInfoTable((InfoTable*)info);
    //printf("\033[0m");

    apk_closures[nargs - 1] = cl;
    apk_return_pcs[nargs - 1] = &code[3];
  }

  *res_clos = apk_closures[nargs - 1];
  *res_pc   = apk_return_pcs[nargs - 1];
}


//--------------------------------------------------------------------

InfoTable *
getAPInfoTable(int nargs)
{
  LC_ASSERT(nargs >= 1 && nargs <= MAX_AP_ARGS);
  if (ap_infos[nargs - 1] != NULL)
    return ap_infos[nargs - 1];

  int codesize =
    1 +     // FUNC
    4 +     // load and evaluate function
    nargs + // load arguments
    1;      // CALLT
  BCIns *code = xmalloc(sizeof(BCIns) * codesize);
  code[0] = BCINS_AD(BC_FUNC, nargs + 1, 0);
  code[1] = BCINS_AD(BC_LOADFV, nargs, 1); // load function ...
  code[2] = BCINS_AD(BC_EVAL, nargs, 0);   // and evaluate it
  code[3] = nargs << 1; // liveness mask
  code[4] = BCINS_AD(BC_MOV_RES, nargs, 0);
  int i;
  for (i = 0; i < nargs; i++)
    code[i + 5] = BCINS_AD(BC_LOADFV, i, i + 2); // load each argument
  // finally, tailcall rN(r0, ..., r{N-1})
  code[nargs + 5] = BCINS_AD(BC_CALLT, nargs, nargs);

  ThunkInfoTable *info = allocInfoTable(wordsof(ThunkInfoTable));
  info->i.type = THUNK;
  info->i.tagOrBitmap = (1 << (nargs + 1)) - 1;
  info->i.layout.payload.ptrs = nargs + 1;
  info->i.layout.payload.nptrs = 0;
  asprintf(&info->name, "stg_AP%d_info", nargs);
  info->code.framesize = nargs + 1;
  info->code.arity = 0;
  info->code.sizelits = 0;
  info->code.sizecode = codesize;
  info->code.lits = NULL;
  info->code.littypes = NULL;
  info->code.code = code;

#if 0
  printf("\033[34mCreated info table: %s (%p)\n", info->name, info);
  printInfoTable((InfoTable*)info);
  printf("\033[0m");
#endif

  ap_infos[nargs - 1] = (InfoTable*)info;
  return (InfoTable*)info;
}
