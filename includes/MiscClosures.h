#ifndef _LAMBDACHINE_MISCCLOSURES_H
#define _LAMBDACHINE_MISCCLOSURES_H

#include "InfoTables.h"
#include "Bytecode.h"

typedef struct _IntClosure {
  const ConInfoTable *info;
  Word val;
} IntClosure;

extern ConInfoTable stg_IND_info;
extern Closure stg_STOP_closure;
extern Closure stg_UPD_closure;
extern BCIns* stg_UPD_return_pc;
extern IntClosure the_smallInt[256];
extern ConInfoTable stg_Izh_con_info;
extern ThunkInfoTable stg_BLACKHOLE_info;
extern Closure stg_BLACKHOLE_closure;
extern PapInfoTable stg_PAP_info;

#define MAX_APK_ARITY           BCMAX_CALL_ARGS

#define MAX_AP_ARGS             BCMAX_CALL_ARGS

void initMiscClosures(void);
void getApContClosure(Closure **res_clos /*out*/, BCIns **res_pc  /*out*/,
                      int nargs, u4 pointer_mask);
InfoTable *getApInfoTable(int nargs, u4 pointer_mask);
void dumpApClosures(void);

#define smallInt(i) (the_smallInt[(i)+128])

#endif
