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
  printf("    1-15:");
  for (i = 0; i < 15; i++) {
    printf("%c%" FMT_Word64, (i % 5) == 0 ? ' ' : '/', buckets[i]);
  }
  printf(" 16+: %" FMT_Word64 "\n\n", buckets[15]);
}

void
printEvents()
{
  printf("-- Stats --------------------------------------\n");
  printf("ALU ops:           %9" FMT_Word64 "\n", G_events.counters[EV_ALU]);
  printf("Multiplications:   %9" FMT_Word64 "\n", G_events.counters[EV_MUL]);
  printf("Comparisions:      %9" FMT_Word64 "\n", G_events.counters[EV_CMP]);
  printf("Loads:             %9" FMT_Word64 "\n", G_events.counters[EV_LOAD]);
  printf("Updates:           %9" FMT_Word64 "\n", G_events.counters[EV_UPDATE]);
  printf("Eval (in HNF):     %9" FMT_Word64 "\n", G_events.counters[EV_EVAL_HNF]);
  printf("Eval (thunk):      %9" FMT_Word64 "\n", G_events.counters[EV_EVAL_THUNK]);
  printf("Function calls:    %9" FMT_Word64 "\n", G_events.counters[EV_CALL]);
  printf("  Arguments:\n");
  printBuckets(G_events.callargs);
  printf("Case dispatches:   %9" FMT_Word64 "\n", G_events.counters[EV_CASE]);
  printf("  Choices:\n");
  printBuckets(G_events.caseargs);
  printf("Allocations:       %9" FMT_Word64 "\n", G_events.counters[EV_ALLOC]);
  printf("  Total words:     %9" FMT_Word64 "\n", G_events.totalalloc);
  printf("  Sizes:\n");
  printBuckets(G_events.allocsize);
  printf("Interpreted:       %9" FMT_Word64 "\n", G_events.counters[EV_DISPATCH]);
  printf("  in record mode:  %9" FMT_Word64 "\n", G_events.counters[EV_RECORD]);
  printf("Traces entered:    %9" FMT_Word64 "\n", G_events.counters[EV_TRACE]);
  printf("  Exit work:\n");
  printBuckets(G_events.exitwork);
  printf("Traces aborted:    %9" FMT_Word64 "\n", G_events.counters[EV_ABORT_TRACE]);
  printf("-----------------------------------------------\n");
}

LC_FASTCALL void
recordEvent_(EventType ev, u4 data)
{
  if (ev == EV_ALLOC) G_events.totalalloc += (u8)data;
  if (data > 16) data = 16;
  switch (ev) {
  case EV_CALL: G_events.callargs[data - 1]++; break;
  case EV_CASE: G_events.caseargs[data - 1]++; break;
  case EV_EXIT: G_events.exitwork[data - 1]++; break;
  case EV_ALLOC: G_events.allocsize[data - 1]++; break;
  default: LC_ASSERT(0); break;
  }
}
