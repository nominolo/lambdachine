#include "common.hh"
#include "jit.hh"
#include "thread.hh"
#include "assembler.hh"

#include <iostream>

using namespace std;

_START_LAMBDACHINE_NAMESPACE


extern "C" void LC_USED
exitTrace(ExitNo n, ExitState *s) {
  if (LC_UNLIKELY(s->F == NULL))
    cerr << "No fragment, skipping snapshot restore.\n";
  else
    s->F->restoreSnapshot(n, s);
}

#define ASM_ENTER NAME_PREFIX "asmEnter"
#define ASM_EXIT  NAME_PREFIX "asmExit"
#define ASM_TRACE NAME_PREFIX "asmTrace"

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

    "movq 16(%%rsp),%%r10\n\t"  /* r10 = code :: (MCode *) */

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
    "movq %%r9,-72(%%rax)\n\t"  /* StackLim */
    "movq %%r8,-80(%%rax)\n\t"  /* HpLim */
    /* &HpLim = [rsp + 0] */

    /* load base pointer */
    "movq %%rsi,%%rbp\n\t"   /* rbp = &T         :: (Thread *)*/
    "addq %1,%%rbp\n\t"      /* rbp = &T.base    :: (Word **) */
    "movq (%%rbp),%%rbp\n\t" /* rbp = base       :: (Word *)  */
    //    "subq %2,%%rbp\n\t"      /* rbp = T->base - 1:: (Word *)  */

    /* load heap pointer */
    "movq %%rcx,%%r12\n\t"   /* r12 = hp */

    /* jump to code */
    "jmp *%%r10\n\t"

    : :
    "i"(SAVE_SIZE /*stack frame size*/), /* %0 */
    "i"(offsetof(Thread, base_)), /* %1 */
    "i"(sizeof(Word))       /* %2 */
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

static void LC_USED
asmTraceIsImplementedInAssembly(void) {
  asm volatile(
    ".globl " ASM_TRACE "\n"
    ASM_TRACE ":\n\t"

    // Stack is unaligned at this point, but we don't have a free
    // register. We align the stack later, before calling another
    // function.

    // "push %%rbp\n\t"            // Align stack
    //"mov %%rsp, %%rbp\n\t"

    /* Save all the registers 16 * 8 = 128 bytes */
    "push %%r14\n\t"
    "movq 8(%%rsp), %%r14\n\t"  // Save return address in r14
    "movq %%r15, 8(%%rsp)\n\t"  // Replace return address with r15
    "push %%r13\n\t"
    "push %%r12\n\t"
    "push %%r11\n\t"
    "push %%r10\n\t"
    "push %%r9\n\t"
    "push %%r8\n\t"
    "push %%rdi\n\t"
    "push %%rsi\n\t"
    "push %%rbp\n\t"
    "pushq %%r14\n\t"  // store return address in place of %rsp
    "push %%rbx\n\t"
    "push %%rdx\n\t"
    "push %%rcx\n\t"
    "push %%rax\n\t"

    // TODO: Build proper C frame?

    /* Make room for xmm registers.  We only save the lower 8 bytes of
       the 16 byte registers, so we only need 16 * 8 = 128 bytes */
    "subq $128, %%rsp\n\t"

#if 0
    // TODO: Actually write XMM registers here.
#endif

    "movq %%rsp, %%rdi\n\t"  // Pointer to exit state as 1st argument
    "call " NAME_PREFIX "debugTrace\n\t"
    "addq $256, %%rsp\n\t"

    // Restore caller-save registers,
    //  -8(rsp) = r15
    // -16(rsp) = r14
    // -24(rsp) = r13
    // -32(rsp) = r12
    "movq -40(%%rsp), %%r11\n\t"
    "movq -48(%%rsp), %%r10\n\t"
    "movq -56(%%rsp), %%r9\n\t"
    "movq -64(%%rsp), %%r8\n\t"
    "movq -72(%%rsp), %%rdi\n\t"
    "movq -80(%%rsp), %%rsi\n\t"
    //    "movq -88(%%rsp), %%rbp\n\t"   callee-save
    // rsp is the return address
    "movq -96(%%rsp), %%rax\n\t"
    //    "movq -104(%%rsp), %%rbx\n\t"  callee-save
    "movq -112(%%rsp), %%rdx\n\t"
    "movq -120(%%rsp), %%rcx\n\t"
    "movq %%rax, -8(%%rsp)\n\t" // Restore return address
    "movq -128(%%rsp), %%rax\n\t"
    "subq $8, %%rsp\n\t"
    "ret\n\t"

    : : );
}

_END_LAMBDACHINE_NAMESPACE
