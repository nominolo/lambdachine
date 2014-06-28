#include "Bitset.h"

#include <stdio.h>

void
printBitset(FILE *stream, Bitset b[], u4 size)
{
  int i;
  char sep = '{';
  for (i = size - 1; i >= 0; i--) {
    if (getBit(b, i)) {
      fprintf(stream, "%c%d", sep, i); sep = ',';
    }
  }
  if (sep == '{')
    fprintf(stream, "{}\n");
  else
    fprintf(stream, "}\n");
}
