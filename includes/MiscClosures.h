#ifndef _LAMBDACHINE_MISCCLOSURES_H
#define _LAMBDACHINE_MISCCLOSURES_H

#include "InfoTables.h"
#include "Bytecode.h"

typedef struct _IntClosure {
  const ConInfoTable *info;
  Word val;
} IntClosure;

extern LcInfoTable stg_IND_info;
extern Closure stg_STOP_closure;
extern Closure stg_UPD_closure;
extern BCIns* stg_UPD_return_pc;
extern IntClosure the_smallInt[256];
extern ConInfoTable stg_Izh_con_info;
extern Closure stg_BLACKHOLE_closure;
extern PapInfoTable stg_PAP_info;

#define MAX_APK_ARITY           (BCMAX_CALL_ARGS - 1)

#define MAX_AP_ARGS             16

void initAPClosures();
void getAPKClosure(Closure **/*out*/, BCIns**/*out*/, int nargs);
InfoTable* getAPInfoTable(int nargs);

#define smallInt(i) (the_smallInt[(i)+128])

#endif
