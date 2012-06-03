#include "fileutils.hh"

namespace lambdachine {

uint32_t fget_u4(FILE *f) {
  uint32_t hh = fget_u1(f);
  uint32_t hl = fget_u1(f);
  uint32_t lh = fget_u1(f);
  uint32_t ll = fget_u1(f);
  return MSB_u4(hh, hl, lh, ll);
}

/* Decode an unsigned variable length integer. */
Word fget_varuint_slow(FILE *f, Word i) {
  uint8_t shift = 7;
  Word b;
  do {
    b = fget_u1(f);
    i = i | (b & 0x7f) << shift;
    shift += 7;
  } while (b & 0x80);

  return i;
}

char *fget_string(FILE *f) {
  uint32_t len = fget_varuint(f);
  char *str = new char[len + 1];
  fread(str, 1, len, f);
  str[len] = '\0';
  return str;
}

bool fileExists(const char *path) {
  struct stat st;
  if (stat(path, &st) != 0) return 0;
  return S_ISREG(st.st_mode) != 0;
}

}
