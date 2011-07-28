
#include "Capability.h"
#include "Jit.h"
#include "InterpAsm.h"
#include "AsmCodeGen.h"
#include "AsmTarget.h" // for exitstub_addr
#include "AsmTarget_x64.h" // for EXITSTUB_SPACING

static void runTrace(JitState *J, Fragment *F);
static void asmEnter(MCode* code);
static void dumpAsm(MCode* mcode, MSize sz, FILE* out);
static void dumpExitStubs(JitState *J);

void asmEngine(Capability *cap, Fragment *F) {
  JitState *J = &cap->J;

  genAsm(J, F);
  dumpAsm(F->mcode, F->szmcode, NULL);
  dumpExitStubs(J);
  LC_ASSERT(0 && "STOP HERE");
  runTrace(&cap->J, F);
}

static void dumpAsm(MCode* mcode, MSize sz, FILE* out) {
  MSize i;
  int close = 0;
  if(out == NULL) {out = fopen("dump.s", "w"); close = 1;};
  fprintf(out, ".text\n");
  for(i = 0; i < sz; i++) {
    fprintf(out, "\t.byte 0x%x\n", mcode[i]);
  }
  fprintf(out, "\n");
  if(close){fclose(out);}
}

static void dumpExitStubs(JitState *J) {
  ExitNo i;
  FILE* out = fopen("dump.s", "a");
  fprintf(out, "\n#\n# EXIT STUBS\n#\n");
  for(i = 0; i < LC_MAX_EXITSTUBGR; i++) {
    if(J->exitstubgroup[i] != NULL) {
      MCode *code = exitstub_addr(J, i);
      dumpAsm(code, EXITSTUB_SPACING * EXITSTUBS_PER_GROUP + 5, out);
        /* +5 for the jump to asmExit at the end of the exit group */
    }
  }
  fprintf(out, "\n");
  fclose(out);
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
