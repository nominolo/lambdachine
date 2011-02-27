#ifndef _LAMBDACHINE_STATS_H
#define _LAMBDACHINE_STATS_H

#include "Common.h"

typedef enum {
  EV_ALU = 1,
  EV_CMP,
  EV_MUL,
  EV_LOAD,
  EV_UPDATE,
  EV_EVAL_HNF,
  EV_EVAL_THUNK,

  EV_DISPATCH,
  EV_NATIVE,
  EV_RECORD,
  EV_TRACE,
  EV_ABORT_TRACE,

  // With additional info counters
  EV_CALL,
  EV_CASE,
  EV_ALLOC,
  EV_EXIT,

  // Keep this
  EV__MAX
} EventType;

typedef struct {
  u8 counters[EV__MAX];
  u8 callargs[16];  // 1,2,...,>=16
  u8 allocsize[16]; // 1,2,...,>=16
  u8 totalalloc;
  u8 caseargs[16];  // 1,2,...,>=16
  u8 exitwork[16];
} Events;

extern Events G_events;

void initEvents();
void printEvents();
LC_FASTCALL void recordEvent_(EventType ev, u4 data);

INLINE_HEADER void recordEvent(EventType ev, u4 data)
{
  G_events.counters[ev]++;
  if (ev >= EV_CALL) recordEvent_(ev, data);
}

#endif
