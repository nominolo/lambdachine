#include "ir.hh"

#include <stdio.h>

int main(int argc, char *argv[]) {
  if (argc < 2) return 1;
  FILE *f = fopen(argv[1], "w");

  fputs("/* DO NOT MODIFY: This file is auto-generated. */\n"
        "#ifndef _FOLD_IROP_MACROS_H\n"
        "#define _FOLD_IROP_MACROS_H\n\n", f);
#define IRPAT(name, flags, arg1, arg2) \
  fprintf(f, "#define " #name "(ref, opc) ((opc)==IR::k" #name ")\n");
  IRDEF(IRPAT);
#undef IRPAT

  fputs("#endif\n", f);
  fclose(f);

  return 0;
}
