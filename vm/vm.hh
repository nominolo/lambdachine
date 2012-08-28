#ifndef _VM_H_
#define _VM_H_

// Forward declarations.

#include "common.hh"

_START_LAMBDACHINE_NAMESPACE

typedef struct _Thread Thread;
class Capability;
class BcIns;
class Jit;  // Forward decl.
class Fragment;
typedef char MCode;
typedef uint32_t ExitNo;
typedef uint32_t TraceId;

_END_LAMBDACHINE_NAMESPACE

#endif /* _VM_H_ */
