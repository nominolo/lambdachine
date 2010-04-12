#include <stdio.h>
#include <stddef.h>

#include "vm.h"

int main()
{
  FILE *file;
  file = fopen("asm_offsets.incl", "w");
  fprintf(file, "#define OFS_SAVED_CSTACK %ld\n",offsetof(VMState, saved_cstack));
  fprintf(file, "#define OFS_L_BASE %ld\n", offsetof(VMState, base));
  fprintf(file, "#define OFS_L_TOP %ld\n", offsetof(VMState, top));
  fprintf(file, "#define OFS_GBL_STATE %ld\n", offsetof(VMState, gbl_state));
  fprintf(file, "#define OFS_SAVEDPC %ld\n", offsetof(VMState, savedpc));
  fprintf(file, "#define OFS_DISPATCH_TBL %ld\n", offsetof(GblState, dispatch_tbl));
  fprintf(file, "#define OFS_L_MAXSTACK %ld\n", offsetof(VMState, pc_top));
  fprintf(file, "#define OFS_FUNC_PC %ld\n", offsetof(FunClosure, proto));
  fprintf(file, "#define OFS_PROTO_K %ld\n", offsetof(Prototype, constants));
  fprintf(file, "#define OFS_PROTO_CODE %ld\n", offsetof(Prototype, code));
  fclose(file);

  return 0;
}
