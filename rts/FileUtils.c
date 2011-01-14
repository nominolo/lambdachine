#include "Common.h"
#include "FileUtils.h"
#include <stdlib.h>

u4
fget_u4(FILE *f)
{
  u4 hh = fget_u1(f);
  u4 hl = fget_u1(f);
  u4 lh = fget_u1(f);
  u4 ll = fget_u1(f);
  return MSB_u4(hh, hl, lh, ll);
}

/* Decode an unsigned variable length integer. */
Word
fget_varuint_slow(FILE *f, Word i)
{
  u1 shift = 7;
  Word b;
  do {
    b = fget_u1(f);
    i = i | (b & 0x7f) << shift;
    shift += 7;
  } while (b & 0x80);

  return i;
}

char *
fget_string(FILE *f)
{
  u4 len = fget_varuint(f);
  char *str = malloc(len + 1);
  fread(str, 1, len, f);
  str[len] = '\0';
  return str;
}
