#include "Common.h"
#include "Jit.h"
#include "Capability.h"
#include "Thread.h"
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


/**
 * Called whenever the interpreter performs a non-local jump (i.e.,
 * CALL[T], EVAL).
 *
 * Takes care of incrementing hotness counters and switching the
 * interpreter mode.
 *
 * @return address where to continue interpreting.
 */
BCIns *
interpreterBranch(Capability *cap, JitState *J, BCIns *src_pc,
                  BCIns *dst_pc, Word *base, BranchType branchType)
{
  if (LC_UNLIKELY(J->mode == JIT_MODE_RECORDING)) {
    /* TODO: check for recording termination. */
    return dst_pc;
  } else {
    if (dst_pc < src_pc && bc_op(*dst_pc) != BC_JFUNC) {
      if (incrementHotCounter(cap, J, dst_pc) && !(cap->flags & CF_NO_JIT)) {
        /* It's hot now. */

        /* Start recording mode. */
        Thread *T = cap->T;
        cap->dispatch = cap->dispatch_record;
        T->pc = dst_pc;
        TraceType traceType =
          branchType == BRANCH_RETURN ? RETURN_TRACE : FUNCTION_TRACE;
        startRecording(J, dst_pc, T, base, traceType);

        return cap->reload_state_pc;
      }
    }
    return dst_pc;
  }
}
