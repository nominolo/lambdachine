/*
**
** Defines various closure used by the runtime system.
**
*/

#include "Bytecode.h"
#include "InfoTables.h"
#include "MiscClosures.h"
#include "PrintClosure.h"

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

InfoTable stg_IND_info = DEF_INFO_TABLE(IND, 0, 1, 0);

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
    3. CALLT rN, N, r0    ; r0 is always the first argument
    framesize = N + 1
    arity = N  (though, unused)

  info table:
    ptr/nptr:  currently we have ptr = N, nptr = 0.  (see TODO above)
    type: FUN
    tag:  0


*/

Closure *ap_closures[MAX_AP_ARITY];
BCIns *ap_return_pcs[MAX_AP_ARITY];

void initAPClosures()
{
  int i;
  for (i = 0; i < MAX_AP_ARITY; i++) {
    ap_closures[i] = NULL;
    ap_return_pcs[i] = NULL;
  }
}

void
getAPClosure(Closure **res_clos, BCIns **res_pc, int nargs)
{
  LC_ASSERT(nargs >= 1 && nargs <= MAX_AP_ARITY + 1);

  if (ap_closures[nargs - 1] == NULL) { // TODO: annotate as unlikely
    // Create closure

    FuncInfoTable *info = malloc(sizeof(FuncInfoTable));
    info->i.type = FUN;
    info->i.tagOrBitmap = 0;
    info->i.layout.payload.ptrs = nargs;  // TODO: see comments above
    info->i.layout.payload.nptrs = 0;
    asprintf(&info->name, "stg_AP%d_info", nargs);
    info->code.framesize = nargs + 1;
    info->code.arity = nargs;
    info->code.sizelits = 0;
    info->code.sizecode = 4 + BC_ROUND(nargs - 1);
    info->code.lits = NULL;
    info->code.littypes = NULL;
    info->code.code = malloc(info->code.sizecode * sizeof(BCIns));

    BCIns *code = info->code.code;

    // The EVAL at the beginning is never executed.  It is only a place to
    // attach the liveness mask and to make sure the closure code can be
    // printed.
    code[0] = BCINS_AD(BC_EVAL, nargs, 0);
    code[1] = cast(BCIns, (1 << nargs) - 1); // liveness mask
    code[2] = BCINS_AD(BC_MOV_RES, nargs, 0); // rN = result
    code[3] = BCINS_ABC(BC_CALLT, nargs, nargs, 0);
    u1 i;
    u1 *args = (u1*)(&code[4]);
    for (i = 1; i < nargs; i++, args++) {
      *args = i;
    }

    Closure *cl = malloc(sizeof(ClosureHeader));  // no payload
    setInfo(cl, (InfoTable*)info);

    //printf("\033[34mCreated closure: %s\n", info->name);
    //printInfoTable((InfoTable*)info);
    //printf("\033[0m");

    ap_closures[nargs - 1] = cl;
    ap_return_pcs[nargs - 1] = &code[2];
  }

  *res_clos = ap_closures[nargs - 1];
  *res_pc   = ap_return_pcs[nargs - 1];
}
