#ifndef _LAMBDACHINE_SNAPSHOT_H
#define _LAMBDACHINE_SNAPSHOT_H

#include "Common.h"
#include "Jit.h"

void growSnapshotBuffer_(JitState *J, Word needed);
void growSnapshotMapBuffer_(JitState *J, Word needed);
void addSnapshot(JitState *J);
void printSnapshot(Fragment *F, SnapShot *snap, SnapEntry *map);
void restoreSnapshot(SnapNo snapno, void *exptr);

INLINE_HEADER void growSnapshotBuffer(JitState *J, Word needed)
{
  if (LC_UNLIKELY(needed > J->sizesnap))
    growSnapshotBuffer_(J, needed);
}

INLINE_HEADER void growSnapshotMapBuffer(JitState *J, Word needed)
{
  if (LC_UNLIKELY(needed > J->sizesnapmap))
    growSnapshotMapBuffer_(J, needed);
}

INLINE_HEADER SnapShot* getSnapshot(Fragment *F, SnapNo snapno)
{
  LC_ASSERT(0 <= snapno && snapno < F->nsnap);
  return &F->snap[snapno];
}

INLINE_HEADER SnapEntry* getSnapshotEntries(Fragment *F, SnapShot *snap)
{
  LC_ASSERT(snap && snap->mapofs < F->nsnapmap);
  return &F->snapmap[snap->mapofs];
}

#endif
