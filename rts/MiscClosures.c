/* 
** 
** Defines various closure used by the runtime system.
**
*/

#include "Bytecode.h"
#include "InfoTables.h"
#include "MiscClosures.h"


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
