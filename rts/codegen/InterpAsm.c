
#include "Capability.h"
#include "Jit.h"
#include "InterpAsm.h"

static void runTrace(JitState *J, Fragment *F);
static void asmEnter(MCode* code);

void asmEngine(Capability *cap, Fragment *F) {
  runTrace(&cap->J, F);
}

static void runTrace(JitState *J, Fragment *F) {
  // Jump to trace machine code
  LC_ASSERT(0 && "Not implemented");

  asmEnter(NULL);
}

static void asmEnter(MCode* code) {
  // In assembly.
  // 1. save caller saves regs
  // 2. switch stack to thread stack
  // 2. setup spill area 256
  // 3. jump to code
}

void asmExit() {
  // In assembly.
  // 1. Create ExitData structure on stack
  // 2. Call exitTrace to restore state
  // 3. Dealloc exit data
  // 4. dealloc spill area
  // 5. switch stack back to C stack
  // 5. return to caller whos return address should be on top of stack
}
