#include "Config.h"
#if LC_HAS_ASM_BACKEND

#include "Capability.h"
#include "Jit.h"
#include "InterpAsm.h"
#include "AsmCodeGen.h"
#include "AsmTarget.h" // for exitstub_addr
#include "AsmTarget_x64.h" // for EXITSTUB_SPACING
#include "Thread.h"
#include "StorageManager.h"
#include "Snapshot.h"

static void enterTrace(JitState *J, Fragment *F);
extern void asmEnter(Fragment *F, Thread *T, Word *spillArea,
                     Word *hp, Word *hplim, Word *stacklim, MCode* code);

void asmEngine(Capability *cap, Fragment *F) {
  JitState *J = &cap->J;

  //exit(42);
  enterTrace(J, F);
}

void dumpAsm(Fragment *F, FILE *out)
{
  MSize i;
  fprintf(out, ".text\nfragment_%d:", (int)F->fragmentid);
  for (i = 0; i < F->szmcode; i++) {
    fprintf(out, "\t.byte 0x%x\n", F->mcode[i]);
  }
  fprintf(out, "\n");
}


void dumpAsmRange(MCode* mcode, MSize sz, FILE* out) {
  MSize i;
  fprintf(out, ".text\n");
  for(i = 0; i < sz; i++) {
    fprintf(out, "\t.byte 0x%x\n", mcode[i]);
  }
  fprintf(out, "\n");
}

void dumpExitStubs(JitState *J, FILE *out) {
  ExitNo i;
  fprintf(out, "\n#\n# EXIT STUBS\n#\n");
  for(i = 0; i < LC_MAX_EXITSTUBGR; i++) {
    if(J->exitstubgroup[i] != NULL) {
      MCode *code = exitstub_addr(J, i);
      fprintf(out, "#Staring addr %p\n", code);
      dumpAsmRange(code, EXITSTUB_SPACING * EXITSTUBS_PER_GROUP + 5, out);
        /* +5 for the jump to asmExit at the end of the exit group */
    }
  }
  fprintf(out, "\n");
}

static void enterTrace(JitState *J, Fragment *F) {
  // Jump to trace machine code
  int spillSizeArea = F->spills;
  Thread *T  = J->T;

  DBG_LVL(2, "framesize = %d, spillsize = %d\n",
	  F->framesize, spillSizeArea);

  // Allocate spill area
  if(stackOverflow(T, T->top, F->framesize + spillSizeArea)) {
    LC_ASSERT(0 && "Stack overflow");
    traceError(NULL, 1);
  }
  T->top += F->framesize + spillSizeArea;
  Word *spillArea = (T->base - 1) + F->framesize;
  Word *hp = G_storage.hp;

  DBG_LVL(1, "Pre trace: Hp = %p, HpLim = %p\n", hp, G_storage.limit);

  asmEnter(F, T, spillArea, hp, G_storage.limit, T->stack + T->stack_size,
	   F->mcode);
}

static void LC_USED
exitTrace(ExitNo n, ExitState* s) {
  restoreSnapshot(n, s);
}

/* Names of the machine code entry and exit functions. These are written in
 * assembly.
 * TODO: only some targets use leading underscores (works on mac os x)*/
#define ASM_ENTER NAME_PREFIX "asmEnter"
#define ASM_EXIT  NAME_PREFIX "asmExit"
/* SAVE_SIZE is the extra stuff that we will keep on the C-stack.
    On entry to the function we automatically save rbp since it is a
    callee saved register and we want to get the stack aligned to 16-bytes. The
    SAVE_SIZE variable is the size of all the other stuff we need to save (not
    including rbp)

    SAVE_SIZE must be multiple of 16 to keep stack alignment. Currently we save

    5 callee saved registers rbx, r12, r13, r14, 15
    1 Fragment pointer
    1 Thread pointer
    1 spill area pointer
    1 Stack Limit pointer
    1 Heap Limit pointer
    --------------------
    10 values * 8 bytes each = 80 bytes

IMPORTANT: When changing this value, make sure to also change [ExitState]
and [HPLIM_SP_OFFS] in AsmTarget_<arch>.h
*/
#define SAVE_SIZE 80

static void LC_USED
asmEnterIsImplementedInAssembly(Fragment *F, Thread *T, Word *spillArea,
                                Word *hp, Word *hplim, Word *stacklim,
                                MCode *code) {
  asm volatile(
    ".globl " ASM_ENTER "\n"
    ASM_ENTER ":\n\t"

     /* save %rbp and also makes %rsp 16-byte aligned */
    "push %%rbp\n\t"

    "movq 16(%%rsp),%%r10\n\t"	/* r10 = code :: (MCode *) */

    /* save other callee saved regs */
    "movq %%rsp, %%rax\n\t"
    "subq %0, %%rsp\n\t"
    "movq %%rbx,-8(%%rax)\n\t"
    "movq %%r12,-16(%%rax)\n\t"
    "movq %%r13,-24(%%rax)\n\t"
    "movq %%r14,-32(%%rax)\n\t"
    "movq %%r15,-40(%%rax)\n\t"

    /* store FTS */
    "movq %%rdi,-48(%%rax)\n\t" /* F */
    "movq %%rsi,-56(%%rax)\n\t" /* T */
    "movq %%rdx,-64(%%rax)\n\t" /* S */
    "movq %%r9,-72(%%rax)\n\t"	/* StackLim */
    "movq %%r8,-80(%%rax)\n\t"  /* HpLim */
    /* &HpLim = [rsp + 0] */

    /* load base pointer */
    "movq %%rsi,%%rbp\n\t"   /* rbp = &T         :: (Thread *)*/
    "addq %1,%%rbp\n\t"      /* rbp = &T.base    :: (Word **) */
    "movq (%%rbp),%%rbp\n\t" /* rbp = base       :: (Word *)  */
    "subq %2,%%rbp\n\t"      /* rbp = T->base - 1:: (Word *)  */

    /* load heap pointer */
    "movq %%rcx,%%r12\n\t"   /* r12 = hp */

    /* jump to code */
    "jmp *%%r10\n\t"

    : : "i"(SAVE_SIZE /*stack frame size*/), /* %0 */
        "i"(offsetof(struct Thread_,base)), /* %1 */
        "i"(sizeof(Word))		    /* %2 */
    );
}

static void LC_USED
asmExitIsImplementedInAssembly() {
  asm volatile(
    ".globl " ASM_EXIT "\n"
    ASM_EXIT  ":\n\t"

    /* construct the ExitState structure on the stack */
    "push %%r13\n\t"
    "push %%r12\n\t"
    "push %%r11\n\t"
    "push %%r10\n\t"
    "push %%r9\n\t"
    "push %%r8\n\t"
    "push %%rdi\n\t"
    "push %%rsi\n\t"
    "push %%rbp\n\t"
      /* save the value of rsp that we ahd during exection of our machine code
       * That rsp was 88 higher than its current level for the 11 pushes
       * we have done since we started exection. Two pushes for the exit number
       * plus the 9 pushes of the registers we have saved so far */
    "lea  88(%%rsp), %%rbp\n\t"
    "push %%rbp\n\t"
    "push %%rbx\n\t"
    "push %%rdx\n\t"
    "push %%rcx\n\t"
    "push %%rax\n\t"

    /* reconstruct the exit number */
    "movzbl -8(%%rbp), %%eax\n\t" /* low byte is exit number */
    "mov   -16(%%rbp), %%ah\n\t" /* high byte is exit group */
    "mov   %%eax, %%edi\n\t"     /* pass as first param to exitTrace funciton */

    /* save remaining registers where the exit number was stored */
    "mov  %%r15, -8(%%rbp)\n\t"
    "mov  %%r14, -16(%%rbp)\n\t"

    /* save the xmm registers */
    "sub   $128, %%rsp\n\t"  /* make room for them on the stack */
    "add  $-128, %%rbp\n\t"  /* skip past the int regs */

    /* For now, we don't actually save the xmm registers because
     * we don't support floating point operations. We still need to
     * make space for them on the stack so that the ExitState data
     * structure is properly constructed. The ExitState has room for
     * the xmm regs, but all the xmm reg data will be invalid. */
#if 0
    "movsd %%xmm15, -8(%%rbp)\n\t"
    "movsd %%xmm14, -16(%%rbp)\n\t"
    "movsd %%xmm13, -24(%%rbp)\n\t"
    "movsd %%xmm12, -32(%%rbp)\n\t"
    "movsd %%xmm11, -40(%%rbp)\n\t"
    "movsd %%xmm10, -48(%%rbp)\n\t"
    "movsd %%xmm9,  -56(%%rbp)\n\t"
    "movsd %%xmm8,  -64(%%rbp)\n\t"
    "movsd %%xmm7,  -72(%%rbp)\n\t"
    "movsd %%xmm6,  -80(%%rbp)\n\t"
    "movsd %%xmm5,  -88(%%rbp)\n\t"
    "movsd %%xmm4,  -96(%%rbp)\n\t"
    "movsd %%xmm3,  -104(%%rbp)\n\t"
    "movsd %%xmm2,  -112(%%rbp)\n\t"
    "movsd %%xmm1,  -120(%%rbp)\n\t"
    "movsd %%xmm0,  -128(%%rbp)\n\t"
#endif

    /* call the generic restore routine exitTrace(ExitNo n, ExitState *s) 
     * rdi = ExitNo
     * rsi = ExitState* (stored on the c-stack */
    "movq %%rsp, %%rsi\n\t"
    "call " NAME_PREFIX "exitTrace\n\t"

    /* deallocate the stack used */
    "addq %0, %%rsp\n\t"

    /* restore callee saved registers */
    "movq -8(%%rsp),%%rbx\n\t"
    "movq -16(%%rsp),%%r12\n\t"
    "movq -24(%%rsp),%%r13\n\t"
    "movq -32(%%rsp),%%r14\n\t"
    "movq -40(%%rsp),%%r15\n\t"

    /* return to original caller */
    "pop %%rbp\n\t"
    "ret\n\t"

    : : "i"(SAVE_SIZE + 256/* 256 bytes for int and float regs
                            * plus the extra space used by SAVE_SIZE */));
}

#endif
