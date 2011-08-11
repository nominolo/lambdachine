#ifndef _LAMBDACHINE_SNAPSHOT_H
#define _LAMBDACHINE_SNAPSHOT_H

#include "Common.h"
#include "Jit.h"

void growSnapshotBuffer_(JitState *J, Word needed);
void growSnapshotMapBuffer_(JitState *J, Word needed);
void addSnapshot(JitState *J);
void printSnapshot(JitState *J, SnapShot *snap, SnapEntry *map);
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

#endif
