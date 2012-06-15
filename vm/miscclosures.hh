#ifndef _MISCCLOSURES_H_
#define _MISCCLOSURES_H_

#include "objects.hh"
#include "memorymanager.hh"

_START_LAMBDACHINE_NAMESPACE

class MiscClosures {
public:
  static void init(MemoryManager &mm);
  static void reset();

  static Closure *stg_UPD_closure_addr;
  static BcIns *stg_UPD_return_pc;
  static Closure *stg_STOP_closure_addr;
  static InfoTable *stg_IND_info;

private:
  static void initStopClosure(MemoryManager &mm);
  static void initUpdateClosure(MemoryManager &mm);
  static void initIndirectionItbl(MemoryManager &mm);
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _MISCCLOSURES_H_ */
