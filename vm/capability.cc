#include "capability.hh"
#include "bytecode.hh"
#include "thread.hh"
#include "objects.hh"

_START_LAMBDACHINE_NAMESPACE

using namespace std;

Capability::Capability() : currentThread_(NULL) {
  interpMsg(kModeInit);
}

Capability::~Capability() {
}

bool Capability::run(Thread *T) {
  LC_ASSERT(T != NULL);
  currentThread_ = T;
  return interpMsg(kModeRun) == kInterpOk;
}

Capability::InterpExitCode Capability::interpMsg(InterpMode mode) {
  static const AsmFunction dispatch_normal[] = {
#   define BCIMPL(name, _) &&op_##name,
    BCDEF(BCIMPL)
#   undef BCIMPL
  };

  if (mode == kModeInit) {
    dispatch_ = dispatch_normal;
    dispatch_normal_ = dispatch_normal;
    return kInterpOk;
  }

  Thread *T;
  const AsmFunction *dispatch;
  Word *base;
  BcIns *pc;
  u4 opA, opB, opC, opcode;
  // Technically, we only need a pointer to the literals.  But having
  // a pointer to the whole code segment can be useful for debugging.
  Code *code = NULL;

# define LOAD_STATE_FROM_CAP \
  do { T = currentThread_; dispatch = dispatch_; \
       base = T->base(); pc = T->pc(); } while (0)

  LOAD_STATE_FROM_CAP;

# define DISPATCH_NEXT \
  opcode = pc->opcode(); \
  opA = pc->a(); \
  opC = pc->d(); \
  ++pc; \
  goto *dispatch[opcode]

# define DECODE_BC \
  opB = opC >> 8; \
  opC = opC & 0xff;

  // Dispatch first instruction.
  DISPATCH_NEXT;

 op_ISLT:
 op_ISGE:
 op_ISLE:
 op_ISGT:
 op_ISEQ:
 op_ISNE:
 op_NOT:
 op_NEG:
 op_MOV:
 op_MOV_RES:
 op_UPDATE:
 op_LOADF:
 op_LOADFV:
 op_LOADBH:
 op_LOADSLF:
 op_INITF:
 op_ADDRR:
  DECODE_BC;
  base[opA] = base[opB] + base[opC];
  DISPATCH_NEXT;

 op_SUBRR:
  DECODE_BC;
  base[opA] = base[opB] - base[opC];
  DISPATCH_NEXT;

 op_MULRR:
 op_DIVRR:
 op_REMRR:
 op_LOADK:
 op_KINT:
 op_NEW_INT:
 op_ALLOC1:
 op_ALLOC:
 op_ALLOCAP:
 op_CALL:
 op_CALLT:
 op_RET1:
 op_JMP:
 op_EVAL:
 op_CASE:
 op_CASE_S:
 op_FUNC:
 op_IFUNC:
 op_JFUNC:
 op_JRET:
 op_IRET:
 op_SYNC:
  cerr << "Unimplemented instruction" << endl;
  return kInterpUnimplemented;

 op_STOP:
  T->sync(pc, base);
  return kInterpOk;
}

_END_LAMBDACHINE_NAMESPACE
