/*
** Machine code management.
** Copyright (C) 2005-2011 Mike Pall. See Copyright Notice in luajit.h
*/

#include "Config.h"
#if LC_HAS_ASM_BACKEND

#include "Jit.h"
#include "MCode.h"
#include "InterpAsm.h" // for reference to asmExitHandler

/* -- OS-specific functions ----------------------------------------------- */
#include <sys/mman.h>

#ifndef MAP_ANONYMOUS
#define MAP_ANONYMOUS	MAP_ANON
#endif

#define MCPROT_RW	(PROT_READ|PROT_WRITE)
#define MCPROT_RX	(PROT_READ|PROT_EXEC)
#define MCPROT_RWX	(PROT_READ|PROT_WRITE|PROT_EXEC)

static void *mcode_alloc_at(JitState *J, uintptr_t hint, size_t sz, int prot)
{
  void *p = mmap((void *)hint, sz, prot, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
  if (p == MAP_FAILED && !hint)
    traceError(J, 1);
  return p;
}

static void mcode_free(JitState *J, void *p, size_t sz)
{
  UNUSED(J);
  munmap(p, sz);
}

static void mcode_setprot(void *p, size_t sz, int prot)
{
  mprotect(p, sz, prot);
}

/* -- MCode area protection ----------------------------------------------- */

/* Define this ONLY if the page protection twiddling becomes a bottleneck. */
#ifdef LUAJIT_UNPROTECT_MCODE

/* It's generally considered to be a potential security risk to have
** pages with simultaneous write *and* execute access in a process.
**
** Do not even think about using this mode for server processes or
** apps handling untrusted external data (such as a browser).
**
** The security risk is not in LuaJIT itself -- but if an adversary finds
** any *other* flaw in your C application logic, then any RWX memory page
** simplifies writing an exploit considerably.
*/
#define MCPROT_GEN	MCPROT_RWX
#define MCPROT_RUN	MCPROT_RWX

static void mcode_protect(JitState *J, int prot)
{
  UNUSED(J); UNUSED(prot);
}

#else

/* This is the default behaviour and much safer:
**
** Most of the time the memory pages holding machine code are executable,
** but NONE of them is writable.
**
** The current memory area is marked read-write (but NOT executable) only
** during the short time window while the assembler generates machine code.
*/
#define MCPROT_GEN	MCPROT_RW
#define MCPROT_RUN	MCPROT_RX

/* Change protection of MCode area. */
static void mcode_protect(JitState *J, int prot)
{
  if (J->mcprot != prot) {
    mcode_setprot(J->mcarea, J->szmcarea, prot);
    J->mcprot = prot;
  }
}

#endif

/* -- MCode area allocation ----------------------------------------------- */

#define mcode_validptr(p)	((p) && (uintptr_t)(p) < (uintptr_t)1<<47)

/* Get memory within relative jump distance of our code in 64 bit mode. */
static void *mcode_alloc(JitState *J, size_t sz)
{
  /* Target an address in the static assembler code (64K aligned).
  ** Try addresses within a distance of target-range/2+1MB..target+range/2-1MB.
  */
  uintptr_t target = (uintptr_t)(void *)asmExit & ~(uintptr_t)0xffff;
  const uintptr_t range = (1u << LC_TARGET_JUMPRANGE) - (1u << 21);
  /* First try a contiguous area below the last one. */
  uintptr_t hint = J->mcarea ? (uintptr_t)J->mcarea - sz : 0;
  int i;
  for (i = 0; i < 32; i++) {  /* 32 attempts ought to be enough ... */
    if (mcode_validptr(hint)) {
      void *p = mcode_alloc_at(J, hint, sz, MCPROT_GEN);

      if (mcode_validptr(p)) {
	if ((uintptr_t)p + sz - target < range || target - (uintptr_t)p < range)
	  return p;
	mcode_free(J, p, sz);  /* Free badly placed area. */
      }
    }
    /* Next try probing pseudo-random addresses. */
    do {
      hint = (0x78fb ^ LC_PRNG_BITS(J, 15)) << 16;  /* 64K aligned. */
    } while (!(hint + sz < range));
    hint = target + hint - (range>>1);
  }
  traceError(J, 1);  /* Give up. OS probably ignores hints? */
  return NULL;
}

/* -- MCode area management ----------------------------------------------- */

/* Linked list of MCode areas. */
typedef struct MCLink {
  MCode *next;		/* Next area. */
  size_t size;		/* Size of current area. */
} MCLink;

/* Allocate a new MCode area. */
static void mcode_allocarea(JitState *J)
{
  MCode *oldarea = J->mcarea;
  size_t sz = (size_t)J->param[JIT_P_sizemcode] << 10;
  sz = (sz + LC_PAGESIZE-1) & ~(size_t)(LC_PAGESIZE - 1);
  J->mcarea = (MCode *)mcode_alloc(J, sz);
  J->szmcarea = sz;
  J->mcprot = MCPROT_GEN;
  J->mctop = (MCode *)((char *)J->mcarea + J->szmcarea);
  J->mcbot = (MCode *)((char *)J->mcarea + sizeof(MCLink));
  ((MCLink *)J->mcarea)->next = oldarea;
  ((MCLink *)J->mcarea)->size = sz;
  J->szallmcarea += sz;
}

/* Free all MCode areas. */
void lj_mcode_free(JitState *J)
{
  MCode *mc = J->mcarea;
  J->mcarea = NULL;
  J->szallmcarea = 0;
  while (mc) {
    MCode *next = ((MCLink *)mc)->next;
    mcode_free(J, mc, ((MCLink *)mc)->size);
    mc = next;
  }
}

/* -- MCode transactions -------------------------------------------------- */

/* Reserve the remainder of the current MCode area. */
MCode *reserveMCode(JitState *J, MCode **lim)
{
  if (!J->mcarea)
    mcode_allocarea(J);
  else
    mcode_protect(J, MCPROT_GEN);
  *lim = J->mcbot;
  return J->mctop;
}

/* Commit the top part of the current MCode area. */
void mcodeCommit(JitState *J, MCode *top)
{
  J->mctop = top;
  mcode_protect(J, MCPROT_RUN);
}

/* Abort the reservation. */
void lj_mcode_abort(JitState *J)
{
  mcode_protect(J, MCPROT_RUN);
}

/* Set/reset protection to allow patching of MCode areas. */
MCode *lj_mcode_patch(JitState *J, MCode *ptr, int finish)
{
#ifdef LUAJIT_UNPROTECT_MCODE
  UNUSED(J); UNUSED(ptr); UNUSED(finish);
  return NULL;
#else
  if (finish) {
    if (J->mcarea == ptr)
      mcode_protect(J, MCPROT_RUN);
    else
      mcode_setprot(ptr, ((MCLink *)ptr)->size, MCPROT_RUN);
    return NULL;
  } else {
    MCode *mc = J->mcarea;
    /* Try current area first to use the protection cache. */
    if (ptr >= mc && ptr < (MCode *)((char *)mc + J->szmcarea)) {
      mcode_protect(J, MCPROT_GEN);
      return mc;
    }
    /* Otherwise search through the list of MCode areas. */
    for (;;) {
      mc = ((MCLink *)mc)->next;
      LC_ASSERT(mc != NULL);
      if (ptr >= mc && ptr < (MCode *)((char *)mc + ((MCLink *)mc)->size)) {
	mcode_setprot(mc, ((MCLink *)mc)->size, MCPROT_GEN);
	return mc;
      }
    }
  }
#endif
}

/* Limit of MCode reservation reached. */
void lj_mcode_limiterr(JitState *J, size_t need)
{
  size_t sizemcode, maxmcode;
  lj_mcode_abort(J);
  sizemcode = (size_t)J->param[JIT_P_sizemcode] << 10;
  sizemcode = (sizemcode + LC_PAGESIZE-1) & ~(size_t)(LC_PAGESIZE - 1);
  maxmcode = (size_t)J->param[JIT_P_maxmcode] << 10;
  if ((size_t)need > sizemcode)
    traceError(J, 1);  /* Too long for any area. */
  if (J->szallmcarea + sizemcode > maxmcode)
    traceError(J, 1);
  mcode_allocarea(J);
  traceError(J, 1);  /* Retry with new area. */
}

#endif
