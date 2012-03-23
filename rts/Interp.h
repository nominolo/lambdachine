#ifndef _LC_INTERP_H_
#define _LC_INTERP_H_

typedef enum {
  INTERP_OK = 0,
  INTERP_OUT_OF_STEPS = 1,
  INTERP_STACK_OVERFLOW = 2,
  INTERP_UNIMPLEMENTED = 3,
} InterpExitCode;

InterpExitCode engine(Capability *);

#endif
