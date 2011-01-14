#ifndef _LAMBDACHINE_MISCCLOSURES_H
#define _LAMBDACHINE_MISCCLOSURES_H

#include "InfoTables.h"

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

#define smallInt(i) (the_smallInt[(i)+128])

#endif
