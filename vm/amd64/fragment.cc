#include "common.hh"
#include "jit.hh"
#include "thread.hh"
#include "assembler.hh"

#include <iostream>

using namespace std;

_START_LAMBDACHINE_NAMESPACE


extern "C" void LC_USED
exitTrace(ExitNo n, ExitState *s) {
  if (LC_UNLIKELY(s->F_id == TRACE_ID_NONE))
    cerr << "No fragment, skipping snapshot restore.\n";
  else {
    Fragment *F = Jit::traceById(s->F_id);
    F->restoreSnapshot(n, s);
  }
}

#define ASM_ENTER NAME_PREFIX "asmEnter"
#define ASM_EXIT  NAME_PREFIX "asmExit"
#define ASM_TRACE NAME_PREFIX "asmTrace"

#define SAVE_SIZE (80 + 256 * sizeof(Word))

/*

Stack Pointer and Signal Handlers
---------------------------------

In general, we have to assume that a signal handler may interrupt us
at any time.  Signal handlers may store some of the user state on
top of the C stack.  This means we have to be very careful about
when we increment or decrement the stack pointer.  For example, the
following can lead to very tricky Heisenbugs (using GNU notation):

    movq %rsp, %rax
    subq 64, %rsp        // make room
    movq %r15, -8(%rax)  // this is fine
    movq %r13, -24(%rax) // this, too
    ...
    addq 64, %rsp         // deallocate space  (a)
    movq -8(%rsp), %r15   // this is fine on POSIX, not Windows  (b)
    movq -24(%rsp), %r13  // this is BAD!  (c)
    ret

The problem in this example is that we use memory below `rsp` is
explicitly defined volatile in the ABI. Anything below `rsp` may be
overwritten anytime by debuggers or system handlers.  On POSIX there
is a 128 byte "red zone" which is guaranteed not be clobbered, but
this is not the case on Windows.

Deallocating stack space must therefore always be done *last*:

    movq 56(%rsp), %r15   // fine
    movq 40(%rsp), %r13   // fine
    addq 64, %rsp         // deallocate space
    ret

Finding such bugs must be extremely tricky (although Valgrind may
help.)  I found it because I read an [article on the x86-64 ABI][1].

 [1]: http://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
  
*/

static void LC_USED
asmEnterIsImplementedInAssembly(TraceId F_id, Thread *T,
                                Word *hp, Word *hplim,
                                Word *stacklim, MCode *code) {
  /* rdi = F_id,  rsi = T,  rdx = hp,  rcx = hplim,
     r8 = stacklim,  r9 = code */
  
  asm volatile(
    ".globl " ASM_ENTER "\n"
    ASM_ENTER ":\n\t"

    /* save %rbp and also makes %rsp 16-byte aligned */
    "push %%rbp\n\t"

    /* save other callee saved regs */
    "movq %%rsp, %%rax\n\t"
    "subq %0, %%rsp\n\t"
    "movq %%rbx,-8(%%rax)\n\t"
    "movq %%r12,-16(%%rax)\n\t"
    "movq %%r13,-24(%%rax)\n\t"
    "movq %%r14,-32(%%rax)\n\t"
    "movq %%r15,-40(%%rax)\n\t"

    /* Make room for spill slots.  It's a lot of memory most of which
       will be used rarely or never.  However, memory that is never
       written or read won't pollute the cache, so it's not a big
       problem.  (We can't put spill slots at addresses below rsp as
       that might be overwritten by signal handlers.)
    */
    "leaq -2048(%%rax), %%rax\n\t"

    /* store FTS */
    "movl %%edi,-48(%%rax)\n\t" /* F_id, NOTE: edi, not rdi */
    "movq %%rsi,-56(%%rax)\n\t" /* T */
    "movq %%r8,-72(%%rax)\n\t"  /* StackLim */
    "movq %%rcx,-80(%%rax)\n\t"  /* HpLim */
    /* &HpLim = [rsp + 0] */

    /* load base pointer */
    "movq %%rsi,%%rbp\n\t"   /* rbp = &T         :: (Thread *)*/
    "addq %1,%%rbp\n\t"      /* rbp = &T.base    :: (Word **) */
    "movq (%%rbp),%%rbp\n\t" /* rbp = base       :: (Word *)  */

    /* load heap pointer */
    "movq %%rdx,%%r12\n\t"   /* r12 = hp */

    /* jump to code */
    "jmp *%%r9\n\t"

    : :
    "i"(SAVE_SIZE /*stack frame size*/), /* %0 */
    "i"(offsetof(Thread, base_)) /* %1 */
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
    "sub   $128, %%rbp\n\t"  /* skip past the int regs */

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

    /* We can't increment the stack pointer just yet. */
    "leaq %c0(%%rsp), %%rax\n\t"

    /* restore callee saved registers */
    "movq -8(%%rax),%%rbx\n\t"
    "movq -16(%%rax),%%r12\n\t"
    "movq -24(%%rax),%%r13\n\t"
    "movq -32(%%rax),%%r14\n\t"
    "movq -40(%%rax),%%r15\n\t"

    /* deallocate the stack used */
    "addq %0, %%rsp\n\t"

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

    /* asmTrace is called, hence the top of the stack currently
       contains the return address.  However, we want to have r15 in
       that place (so we can reuse the ExitState struct).
    */

    /* Save all the registers 16 * 8 = 128 bytes */
    "push %%r14\n\t"
    "movq 8(%%rsp), %%r14\n\t"  // Save return address in r14
    "movq %%r15, 8(%%rsp)\n\t"  // Replace return address with r15

    /* At this point we have r15 and r14 stored at the right places on
       the stack and r14 holds the return address.  Since r14 is
       callee-save it can remain there. */

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

    /* Make room for xmm registers.  We only save the lower 8 bytes of
       the 16 byte registers, so we only need 16 * 8 = 128 bytes */
    "subq $128, %%rsp\n\t"

#if 0
    // TODO: Actually write XMM registers here.
#endif

    /* The stack is 16-byte aligned. */
    "movq %%rsp, %%rdi\n\t"  // Pointer to exit state as 1st argument
    "call " NAME_PREFIX "debugTrace\n\t"

    "addq $128, %%rsp\n\t"  /* deallocate space for xmm registers */
    
    /* r12-r15 are guarenteed to be the same. r14 contains the return
       address. */
    "movq 0(%%rsp), %%rax\n\t"
    "movq 8(%%rsp), %%rcx\n\t"
    "movq 16(%%rsp), %%rdx\n\t"
    /*    "movq 24(%rsp), %%rbx\n\t" // callee-save */ 
    /* "movq 32(%rsp), %%rsp\n\t" */
    "movq 40(%%rsp), %%rbp\n\t"
    "movq 48(%%rsp), %%rsi\n\t"
    "movq 56(%%rsp), %%rdi\n\t"
    "movq 64(%%rsp), %%r8\n\t"
    "movq 72(%%rsp), %%r9\n\t"
    "movq 80(%%rsp), %%r10\n\t"
    "movq 88(%%rsp), %%r11\n\t"
    // "movq 96(%rsp), %%r12\n\t" // callee-save
    // "movq 104(%rsp), %%r13\n\t" // callee-save
    "movq %%r14, 120(%%rsp)\n\t"  // put return address in expected location
    "movq 112(%%rsp), %%r14\n\t"  // restore old value of r14
    // r15 is unmodified
    "addq $120, %%rsp\n\t"  /* Deallocate stack, top of stack is now return address */

    "ret\n\t"

    : : );
}

_END_LAMBDACHINE_NAMESPACE
