#include "fileutils.hh"

_START_LAMBDACHINE_NAMESPACE

bool fileExists(const char *path) {
  struct stat st;
  if (stat(path, &st) != 0) return 0;
  return S_ISREG(st.st_mode) != 0;
}

_END_LAMBDACHINE_NAMESPACE
