#include "Bytecode.h"

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
