#include "Stats.h"

#include <string.h>
#include <stdio.h>

Events G_events;

void
initEvents()
{
  memset(&G_events, 0, sizeof(Events));
}

void
printBuckets(u8 *buckets)
{
  int i;
  fprintf(stderr, "    1-15:");
  for (i = 0; i < 15; i++) {
    fprintf(stderr, "%c%" FMT_Word64, (i % 5) == 0 ? ' ' : '/', buckets[i]);
  }
  fprintf(stderr, " 16+: %" FMT_Word64 "\n\n", buckets[15]);
}

void
printEvents()
{
  fprintf(stderr, "-- Stats --------------------------------------\n");
  fprintf(stderr, "ALU ops:           %9" FMT_Word64 "\n", G_events.counters[EV_ALU]);
  fprintf(stderr, "Multiplications:   %9" FMT_Word64 "\n", G_events.counters[EV_MUL]);
  fprintf(stderr, "Divisions:         %9" FMT_Word64 "\n", G_events.counters[EV_REMDIV]);
  fprintf(stderr, "Comparisions:      %9" FMT_Word64 "\n", G_events.counters[EV_CMP]);
  fprintf(stderr, "Loads:             %9" FMT_Word64 "\n", G_events.counters[EV_LOAD]);
  fprintf(stderr, "Updates:           %9" FMT_Word64 "\n", G_events.counters[EV_UPDATE]);
  fprintf(stderr, "Eval (in HNF):     %9" FMT_Word64 "\n", G_events.counters[EV_EVAL_HNF]);
  fprintf(stderr, "Eval (thunk):      %9" FMT_Word64 "\n", G_events.counters[EV_EVAL_THUNK]);
  fprintf(stderr, "Function calls:    %9" FMT_Word64 "\n", G_events.counters[EV_CALL]);
  fprintf(stderr, "  Arguments:\n");
  printBuckets(G_events.callargs);
  fprintf(stderr, "Case dispatches:   %9" FMT_Word64 "\n", G_events.counters[EV_CASE]);
  fprintf(stderr, "  Choices:\n");
  printBuckets(G_events.caseargs);
  fprintf(stderr, "Allocations:       %9" FMT_Word64 "\n", G_events.counters[EV_ALLOC]);
  fprintf(stderr, "  Total words:     %9" FMT_Word64 "\n", G_events.totalalloc);
  fprintf(stderr, "  Sizes:\n");
  printBuckets(G_events.allocsize);
  fprintf(stderr, "Interpreted:       %9" FMT_Word64 "\n", G_events.counters[EV_DISPATCH]);
  fprintf(stderr, "  in record mode:  %9" FMT_Word64 "\n", G_events.counters[EV_RECORD]);
  fprintf(stderr, "Traces entered:    %9" FMT_Word64 "\n", G_events.counters[EV_TRACE]);
  fprintf(stderr, "  Exit work:\n");
  printBuckets(G_events.exitwork);
  fprintf(stderr, "Traces aborted:    %9" FMT_Word64 "\n", G_events.counters[EV_ABORT_TRACE]);
  fprintf(stderr, "-----------------------------------------------\n");
  fprintf(stderr, "Ticks:             %9" FMT_Word64 "\n", G_events.counters[EV_TICK]);
}

LC_FASTCALL void
recordEvent_(EventType ev, u4 data)
{
  if (ev == EV_ALLOC) G_events.totalalloc += (u8)data;
  if (data > 16) data = 16;
  switch (ev) {
  case EV_CALL: G_events.callargs[data - 1]++; break;
  case EV_CASE: G_events.caseargs[data - 1]++; break;
  case EV_EXIT: if (data > 0) G_events.exitwork[data - 1]++; break;
  case EV_ALLOC: G_events.allocsize[data - 1]++; break;
  default: LC_ASSERT(0); break;
  }
}
