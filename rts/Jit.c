#include "Common.h"
#include "Jit.h"
#include <string.h>
#include <stdio.h>

/**
 * Parse and set a JIT parameter.
 *
 * Parse a string of the form "<param>=<value>" where <param> must be
 * a JIT parameter (see Jit.h) and <value> must be an integer.
 *
 * @returns whether the parameter was parsed and set successfully.
 */
static bool parseJitParam(int32_t *param, const char *str) {
  /* JIT_P_STRING has the form: `(length of following string)(string)` */
  const char *options = JIT_P_STRING;
  int i;
  for (i = 0; i < JIT_P__MAX; i++) {
    size_t len = *(const uint8_t *)options;
    LC_ASSERT(len > 0);
    if (strncmp(str, options + 1, len) == 0 &&
        str[len] == '=') {
      int32_t value = 0;
      const char *p = &str[len+1];
      while (*p >= '0' && *p <= '9') {
        value = value * 10 + (*p++ - '0');
      }
      if (*p != 0) {
        /* Could not parse complete number. */
        return false;
      }
      /* TODO: Validate range */
      param[i] = value;
      return true;
    }
    options += 1 + len;
  }
  return false;
}

/**
 * Parse and set/clear a JIT flag.
 *
 * Parse a string of the form "[+-]?<jit-flag>" and sets or clears the
 * corresponding JIT flag.  For example, "+cse" enables the common subexpression
 * elimination optimisation.  If no prefix is given, sets the flag.
 */
static bool parseJitFlag(uint32_t *flags, const char *str) {
  const char *options = JIT_OPTSTRING;
  uint32_t opt;
  int set = 1;
  if (str[0] == '+') {
    str++;
  } else if (str[0] == '-') {
    str++;
    set = 0;
  }
  for (opt = JIT_OPT_FIRST;  ; opt <<= 1) {
    size_t len = *(const uint8_t *)options;
    if (len == 0)
      break;
    if (strncmp(str, options + 1, len) == 0 && str[len] == '\0') {
      if (set) {
        *flags |= opt;
      } else {
        *flags &= ~opt;
      }
      return true;
    }
    options += 1 + len;
  }
  return false;
}

/**
 * Parse and set a JIT parameter or flag.
 */
bool parseJitOpt(int32_t *param, uint32_t *flags, const char *str) {
  return parseJitFlag(flags, str) || parseJitParam(param, str);
}

void setJitOpts(JitState *J, int32_t *param, uint32_t flags) {
  int i;
  for (i = 0; i < JIT_P__MAX; i++) {
    J->param[i] = param[i];
  }
  J->flags = flags;
}
