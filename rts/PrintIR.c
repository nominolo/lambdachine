#include "PrintIR.h"

#include <stdio.h>

const char *ir_type_names[] = {
  [IRT_I32]  = "i32",
  [IRT_U32]  = "u32",
  [IRT_CHAR] = "chr",
  [IRT_F32]  = "flt",
  [IRT_PTR]  = "ptr",
};

#define STR(x) #x

const char *ir_name[] = {
#define IRNAME(name,flags,mode1,mode2)  [IR_##name] = STR(name),
  IRDEF(IRNAME)
#undef IRNAME
};

const u1 ir_mode[IR__MAX + 1] = {
  IRDEF(IRMODE)
  0
};

INLINE_HEADER const char *irt_str(IRType irt)
{
  return ir_type_names[irt & IRT_TYPE];
}

void printIRRef(JitState *J, IRRef1 ref)
{
  if (ref < REF_BIAS)
    printf("K%03d ", REF_BIAS - ref);
  else
    printf("%04d ", ref - REF_BIAS);
}

void printIR(JitState *J, IRIns ir)
{
  IRRef1 ref;
  printf("%3s %-8s ", irt_str(ir.t), ir_name[ir.o]);
  switch (irm_op1(ir_mode[ir.o])) {
  case IRMref:
    printIRRef(J, ir.op1);
    break;
  case IRMlit:
    if (ir.o == IR_SLOAD)
      printf(" %4d", (IRRef1)ir.op1 - J->baseslot);
    else
      printf(" %4d", (IRRef1)ir.op1);
    break;
  case IRMcst:
    if (ir.o == IR_KWORD) {
      printf("  0x%08" FMT_WordX, J->kwords[ir.u]);
    } else
      printf(" %11d", ir.i);
    break;
  case IRMnone:  printf("     "); break;
  }

  switch (irm_op2(ir_mode[ir.o])) {
  case IRMref:
    ref = (IRRef1)ir.op2;
    if (ref < REF_BIAS)
      printf(" K%03d", REF_BIAS - ref);
    else
      printf(" %04d", ref - REF_BIAS);
    break;
  case IRMlit: printf(" %4d", (IRRef1)ir.op2); break;
  case IRMcst: printf(" %11d", ir.i); break;
  case IRMnone:
    if (irm_op1(ir_mode[ir.o]) != IRMcst) printf("     ");
    break;
  }

  //  printf("  prev: %d", ir.prev ? ir.prev - REF_BIAS : 0);

  printf("\n");
}
