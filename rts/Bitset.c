#include "Bitset.h"

#include <stdio.h>

void
printBitset(Bitset b[], u4 size)
{
  int i;
  char sep = '{';
  for (i = size - 1; i >= 0; i--) {
    if (getBit(b, i)) {
      printf("%c%d", sep, i); sep = ',';
    }
  }
  if (sep == '{')
    printf("{}\n");
  else
    printf("}\n");
}
