#include "bytecode.hh"
#include "objects.hh"

#include <stdio.h>

#define FOO 42

#define STR(x) #x

using namespace lambdachine;

int
main(int argc, char **argv)
{
  printf("#ifndef _LAMBDACHINE_OPCODES_H\n"
         "#define _LAMBDACHINE_OPCODES_H\n\n");
  printf("/* DO NOT EDIT.  This is an auto-generated file. */\n");
  printf("/* See utils/genopcodes.cc */\n\n");

  printf("#define branch_BIAS 0x%x\n", BcIns::kBranchBias);
  printf("#define cMAX_CALL_ARGS %d\n\n", BcIns::kMaxCallArgs);
  
#define DEF_LINE(name,fmt)                                         \
  printf("#define " STR(opc_ ## name) " %d\n", BcIns::k##name);
  
  BCDEF(DEF_LINE)
#undef DEF_LINE  
    ;
  printf("\n");

  printf("#define littype_INT %d\n", LIT_INT);
  printf("#define littype_WORD %d\n", LIT_WORD);
  printf("#define littype_CHAR %d\n", LIT_CHAR);
  printf("#define littype_FLOAT %d\n", LIT_FLOAT);
  printf("#define littype_STRING %d\n", LIT_STRING);
  printf("#define littype_CLOSURE %d\n", LIT_CLOSURE);
  printf("#define littype_INFO %d\n", LIT_INFO);
  printf("\n");
  
  // Closure types
#define DEF_LINE(name, _) \
  printf("#define " STR(cltype_ ## name) " %d\n", name);

  CTDEF(DEF_LINE)
#undef DEF_LINE
    ;

  printf("\n#endif\n");

  return 0;
}
