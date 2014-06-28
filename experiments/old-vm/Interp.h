#ifndef _LC_INTERP_H_
#define _LC_INTERP_H_

typedef enum {
  INTERP_OK = 0,
  INTERP_OUT_OF_STEPS = 1,
  INTERP_STACK_OVERFLOW = 2,
  INTERP_UNIMPLEMENTED = 3,
} InterpExitCode;

typedef enum {
  EM_INIT,
  EM_INTERP
} EngineMessage;

InterpExitCode engine_impl(Capability*, EngineMessage);

INLINE_HEADER InterpExitCode engine(Capability *cap) {
  return engine_impl(cap, EM_INTERP);
}

INLINE_HEADER void initDispatchTables(Capability *cap) {
  engine_impl(cap, EM_INIT);
}

#endif
