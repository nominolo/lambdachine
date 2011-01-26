#include "Bytecode.h"

#include <stdio.h>
#include <stdlib.h>

InsFormat ins_format[] = {
#define BCFMT(name,f)  [BC_##name] = IFM_##f,
  BCDEF(BCFMT)
#undef BCFMT
  [BC__MAX] = IFM____
};

#define STR(x) #x

const char *ins_name[] = {
#define BCFMT(name,fmt)  [BC_##name] = STR(name),
  BCDEF(BCFMT)
#undef BCFMT
  [BC__MAX] = "STOP"
};

u4 instructionSize(const BCIns *p)
{
  BCIns i = *p;
  switch (ins_format[bc_op(i)]) {
  case IFM_RRJ: case IFM_J:
     return 2;
  case IFM____:
    switch (bc_op(i)) {
    case BC_EVAL:    return 2;
    case BC_ALLOC:   return 1 + BC_ROUND(bc_c(i) - 1);
    case BC_ALLOCAP: return 1 + BC_ROUND(bc_c(i));
    case BC_CASE:    return 1 + (bc_d(i) + 1) / 2;
    case BC_CASE_S:  return 1 + bc_d(i);
    case BC_CALL:    return 2 + BC_ROUND(bc_b(i) - 1);
    case BC_CALLT:   return 1 + BC_ROUND(bc_b(i) - 1);
    case BC__MAX:    return 1;
    default:
      fprintf(stderr, "instructionSize: unknown size for opcode %d",
              bc_op(i));
      exit(1);
    }
  default:
    return 1;
  }
}
