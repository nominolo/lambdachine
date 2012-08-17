#ifndef _MISCCLOSURES_H_
#define _MISCCLOSURES_H_

#include "objects.hh"
#include "memorymanager.hh"

#include HASH_MAP_H

_START_LAMBDACHINE_NAMESPACE

class MiscClosures {
public:
  static void init(MemoryManager *mm);
  static void reset();

  /// Get the closure pointer and return address for an application
  /// continuation.  Application continuations are created by the
  /// FUNC instruction if an overapplication is detected.
  ///
  /// @param clos Output parameter.
  /// @param ret_addr Output parameter.
  /// @param nargs Total number of arguments.
  /// @param pointerMask Describes which arguments are pointers.
  static void getApCont(Closure **clos, BcIns **ret_addr,
                        u4 nargs, u4 pointerMask);
  static inline u4 apContFrameSize(u4 nargs) {
    return nargs + 1;
  }
  static InfoTable *getApInfo(u4 nargs, u4 pointerMask);

  static Closure *stg_UPD_closure_addr;
  static BcIns *stg_UPD_return_pc;
  static const uint32_t UPD_frame_size = 2;
  
  static Closure *stg_STOP_closure_addr;
  static InfoTable *stg_IND_info;
  static InfoTable *stg_PAP_info;

private:
  typedef struct {
    Closure *closure;
    BcIns *returnAddr;
  } ApContInfo;

  static const u4 kMaxSmallArity = 4;

  #define APKMAP HASH_NAMESPACE::HASH_MAP_CLASS<u4, ApContInfo>
  #define APMAP HASH_NAMESPACE::HASH_MAP_CLASS<u4, InfoTable*>

  static ApContInfo *smallApConts;
  static APKMAP *otherApConts;
  static MemoryManager *allocMM;
  static InfoTable **smallApInfos;
  static APMAP *otherApInfos;

  static inline u4 apContIndex(u4 nargs, u4 pointerMask) {
    return (1u << nargs) - 2 + pointerMask;
  }
  static void initStopClosure(MemoryManager &mm);
  static void initUpdateClosure(MemoryManager &mm);
  static void initIndirectionItbl(MemoryManager &mm);
  static void initPapItbl(MemoryManager *mm);
  static void initApConts(MemoryManager *mm);
  static void initApInfos(MemoryManager *mm);
  static void buildApCont(MemoryManager *mm, ApContInfo *info,
                          u4 nargs, u4 pointerMask);
  static InfoTable* buildApInfo(MemoryManager *mm, u4 nargs,
                                u4 pointerMask);
};

_END_LAMBDACHINE_NAMESPACE

#endif /* _MISCCLOSURES_H_ */
