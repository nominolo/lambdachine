#include "Common.h"
#include "Jit.h"
#include "Bitset.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef LC_SELF_CHECK_MODE

typedef struct {
  Word *address;
  Word value;
} ShadowHeapEntry;

ShadowHeapEntry *G_shadowHeap;
ShadowHeapEntry *G_shadowHeapTail;
Word *G_shadowHp;
Word *G_shadowHpLim;

typedef struct {
  Word *base;
  Word *top;
  Word *savedStack;
  BCIns *pc;
} ShadowState;

typedef struct {
  Word base;
  u4 size;
  Word *stack;
  Bitset *bitset;
  Word *origStack;
} ShadowStack;

ShadowStack G_shadowStack;

Word
readStack(Word *base, int slot)
{
  Word offset = (base - G_shadowStack.origStack) + slot;
  if (getBit(G_shadowStack.bitset, offset)) {
    DBG_LVL(3, "SHADOW_STACK_READ: %d => %" FMT_WordX "\n",
            (int)offset, G_shadowStack.stack[offset]);
    return G_shadowStack.stack[offset];
  } else {
    DBG_LVL(3, "REAL_STACK_READ: %d/%p => %" FMT_WordX " (base=%p,offs=%d)\n",
            (int)offset, &G_shadowStack.origStack[offset],
            G_shadowStack.origStack[offset],
            base, slot);
    return G_shadowStack.origStack[offset];
  }
}

void
writeStack(Word *base, int slot, Word value)
{
  Word offset = (base - G_shadowStack.origStack) + slot;
  G_shadowStack.stack[offset] = value;
  setBit(G_shadowStack.bitset, offset);
}

void initShadowStack(u4 size, Word *stack, Word *base)
{
  G_shadowStack.size = size;
  G_shadowStack.origStack = stack;
  if (G_shadowStack.stack) {
    xfree(G_shadowStack.stack);
  }
  if (G_shadowStack.bitset) {
    xfree(G_shadowStack.bitset);
  }
  G_shadowStack.stack = xmalloc(size * sizeof(Word));
  G_shadowStack.bitset = xmalloc(BITSET_SIZE(size) * sizeof(Bitset));
  memset(G_shadowStack.stack, 0, size * sizeof(Word));
  clearBitset(G_shadowStack.bitset, size);
  G_shadowStack.base = stack - base;
}

void clearShadowStack(void) {
  clearBitset(G_shadowStack.bitset, G_shadowStack.size);
}

bool checkShadowSlot(Word *slot)
{
  Word offset = slot - G_shadowStack.origStack;
  if (getBit(G_shadowStack.bitset, offset)) {
    return *slot == G_shadowStack.stack[offset];
  } else {
    return true;
  }
}

void initShadowHeap(void)
{
  G_shadowHeap = xmalloc(MAX_SHADOW_HEAP_SIZE * sizeof(*G_shadowHeap));
  memset(G_shadowHeap, 0, MAX_SHADOW_HEAP_SIZE * sizeof(*G_shadowHeap));
  G_shadowHeapTail = G_shadowHeap;
  G_shadowHp = NULL;
  G_shadowHpLim = NULL;

  G_shadowStack.size = 0;
  G_shadowStack.stack = NULL;
  G_shadowStack.bitset = NULL;
  G_shadowStack.origStack = NULL;
}

Word
lookupShadowHeap(Word *address)
{
  ShadowHeapEntry *p;
  /* Try to look up the value in the shadow heap. */
  for (p = G_shadowHeap; p < G_shadowHeapTail; p++) {
    if (address == p->address) {
      return p->value;
    }
  }

  /* Otherwise, return the value at the actual address. */
  return *address;
}

void
writeToShadowHeap(Word *address, Word value)
{
  ShadowHeapEntry *p;
  /* Try to look up the value in the shadow heap. */
  for (p = G_shadowHeap; p < G_shadowHeapTail; p++) {
    if (address == p->address) {
      p->value = value;
      return;
    }
  }
  G_shadowHeapTail->address = address;
  G_shadowHeapTail->value = value;
  ++G_shadowHeapTail;
  if (G_shadowHeapTail >= G_shadowHeap + MAX_SHADOW_HEAP_SIZE) {
    fprintf(stderr, "FATAL: Ran out of space for shadow heap.\n");
    abort();
  }
}

void resetShadowHeap(Word *hp, Word *hp_lim) {
  memset(G_shadowHeap, 0,
	 sizeof(*G_shadowHeap) * (G_shadowHeapTail - G_shadowHeap));
  G_shadowHeapTail = G_shadowHeap;
  G_shadowHp = hp;
  G_shadowHpLim = hp_lim;
}

Word *allocOnShadowHeap(Word nwords)
{
  Word *result = G_shadowHp;
  G_shadowHp += nwords;
  if (G_shadowHp >= G_shadowHpLim) {
    fprintf(stderr, "FATAL: Shadow heap crossed heap limit.\n");
    abort();
  }
  return result;
}

bool verifyShadowHeap() {
  u4 mismatches = 0;
  ShadowHeapEntry *p;
  DBG_LVL(2, ".. Verifying shadow heap (entries: %d)\n",
          (int)(G_shadowHeapTail - G_shadowHeapTail));
  for (p = G_shadowHeap; p < G_shadowHeapTail; p++) {
    Word value = *p->address;
    // TODO: Verify that address points to valid region
    DBG_LVL(3, ".... verifying %p: jit: %9" FMT_WordX " interp: %9" FMT_WordX "\n",
            p->address, p->value, value);
    if (value != p->value) {
      fprintf(stderr, "Heap mismatch at: %p\n"   "  real heap:   %"
              FMT_WordX "  shadow heap: %" FMT_WordX "\n",
	      p->address, value, p->value);
      mismatches++;
    }
  }
  return mismatches == 0;
}

bool verifyShadowStack() {
  u4 mismatches = 0;
  u4 i;
  for (i = 0; i < G_shadowStack.size; i++) {
    if (getBit(G_shadowStack.bitset, i)) {
      if (G_shadowStack.stack[i] != G_shadowStack.origStack[i]) {
        mismatches++;
        fprintf(stderr, "Stack mismatch: %p expected: %"
                FMT_WordX " got: %" FMT_WordX "\n",
                &G_shadowStack.origStack[i],
                G_shadowStack.origStack[i],
                G_shadowStack.stack[i]);
      }
    }
  }
  return mismatches == 0;
}

void printShadowHeap(FILE *stream) {
  ShadowHeapEntry *p;
  int i = 1;
  for (p = G_shadowHeap; p < G_shadowHeapTail; p++) {
    if ((i % 4) == 0) {
      fprintf(stream, "\n");
    }
    fprintf(stream, "%p: %" FMT_WordX "  ", p->address, p->value);
    ++i;
  }
  fprintf(stream, "\n");
  fflush(stream);
}

#endif
