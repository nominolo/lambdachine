// Byte code interpreter (x86-64 only, for now)
#include "asm_offsets.incl"
#include "opcodes.h"

#define X64_POSIX       1

        .text
	.align 4,0x90

#ifdef X64_WIN
/*

x64/Windows calling convention:

Callee save: rbx, rsi, rdi, rbp, rsp, r12, r13, r14, r15
Caller save: rax, rcx, rdx, r8, r9, r10, r11
Params:  rcx, rdx, r8, r9, stack(right to left)

*/

#elif X64_POSIX
/*

x64/POSIX calling convention:

Callee save: rbx, rbp, r12, rsp, r13, r14, r15
Caller save: rax, rcx, rdx, rsi, rdi, r8, r9, r10, r11
Params:  rdi, rsi, rdx, rcx, r8, r9, stack(right to left)

*/
#define DWORD           4
#define QWORD           8
#define MWORD           QWORD

// All callee save
#define BASE            %rdx
#define KBASE           %rdi
#define PC              %rsi
#define DISPATCH        %rbx

#define RET_TOP         %r14
#define LSTATE          %r15
#define NODE            %r8

#define RA              %rcx
#define RAH             %ch
#define RAL             %cl
#define RB              %rbp
#define RC              %rax
#define RCW             %ax
#define RCH		%ah
#define RCL		%al

#define OP		RB
#define RD		RC
#define RDW		RCW
#define RDL		RCL


#define RAd             %ecx
#define RBd             %ebp
#define RCd             %eax
#define RDd             %eax
#define OPd             RBd

#define CARG1		%rdi		// x64/POSIX C call arguments.
#define CARG2		%rsi
#define CARG3		%rdx
#define CARG4		%rcx
#define CARG5		%r8
#define CARG6		%r9
#define CARG1d          %edi
#define CARG2d	        %esi
#define CARG3d	        %edx
#define CARG4d          %ecx
#define CARG5d          %r8d
#define CARG6d          %r9d


#define BCBIAS_J	0x8000

#define CFRAME_SPACE    QWORD*5

        .macro save_regs
        pushq   %rbp
        pushq   %rbx
        pushq   %r15
        pushq   %r14
        subq    $(CFRAME_SPACE), %rsp
        .endm

        .macro restore_regs
        addq    $(CFRAME_SPACE), %rsp
        popq    %r14
        popq    %r15
        popq    %rbx
        popq    %rbp
        .endm

// Stack frame layout:
//
//      --------- 16 byte aligned ----------
//	[rsp+72]	saved return address (from C code) / SAVE_RET
//      [rsp+64]	saved rbp
//      [rsp+56]	saved rbx
//      [rsp+48]	saved r15
//	[rsp+40]	saved r14
//	[rsp+32]        SAVE_L     thread-local state
//	[rsp+24]
//	[rsp+16]
//	[rsp+8]
//	[rsp]
//      --------- 16 byte aligned ----------

#define SAVE_RET        (QWORD*9)(%rsp)
#define SAVE_R4         (QWORD*8)(%rsp)
#define SAVE_R3         (QWORD*7)(%rsp)
#define SAVE_R2         (QWORD*6)(%rsp)
#define SAVE_R1         (QWORD*5)(%rsp)
#define SAVE_L          (QWORD*4)(%rsp)
// The rest is currently undefined

#endif

	// Instruction decode headers for various instruction formats.
	.macro ins_A
	.endm

	.macro ins_AD
	.endm

	.macro ins_AJ
	.endm

	.macro ins_ABC
	movzx	RCH, RBd
	movzx	RCL, RCd
	.endm

	.macro ins_AB_
	movzx	RCH, RBd
	.endm

	.macro ins_A_C
	movzx	RCL, RCd
	.endm

	// Load new PC from reg (relative address)
	.macro branchPC // argument is the register to be used
	leaq	-(BCBIAS_J*4)(PC,$0,4), PC
	.endm

	// Two possible implementations.  Both require 3 bytes.
	//
	// TODO: benchmark which one works best (likely to be arch-dependent)
	.macro inc_PC
	//leal	4(PC), PC
	addq	$(4), PC
	.endm

	.macro ins_next
        movl    (PC), RCd   // high 32 bits of RC are zero-extended
        movzx   RCH, RAd
        movzx   RCL, OPd
        inc_PC
        shrl    $(16), RCd
        jmp     *(DISPATCH,OP,MWORD)
	.endm

        .macro ins_callt
        movl    (PC), RAd
        movzx   RAL, OPd  // overwrites RB (OP = RB)
        movzx   RAH, RAd
        inc_PC
        jmp     *(DISPATCH,OP,MWORD)
        .endm

.globl _vm_init_dispatch
_vm_init_dispatch:
        // CARG1 = AsmFn* dispatch_table;
        movq    CARG1, %rdx
        leaq    _bci_exit(%rip), %rax
        movq    %rax, (MWORD*OP_EXIT)(%rdx)
        leaq    _bci_addi_rr(%rip), %rax
        movq    %rax, (MWORD*OP_ADDI_RR)(%rdx)
        leaq    _bci_subi_rr(%rip), %rax
        movq    %rax, (MWORD*OP_SUBI_RR)(%rdx)
        leaq    _bci_muli_rr(%rip), %rax
        movq    %rax, (MWORD*OP_MULI_RR)(%rdx)
        leaq    _bci_kshort(%rip), %rax
        movq    %rax, (MWORD*OP_KSHORT)(%rdx)
        leaq    _bci_mov_rk(%rip), %rax
        movq    %rax, (MWORD*OP_LOADK)(%rdx)
        leaq    _bci_jmp(%rip), %rax
        movq    %rax, (MWORD*OP_JMP)(%rdx)
        leaq    _bci_islt(%rip), %rax
        movq    %rax, (MWORD*OP_ISLT)(%rdx)
        leaq    _bci_isge(%rip), %rax
        movq    %rax, (MWORD*OP_ISGE)(%rdx)
        leaq    _bci_isle(%rip), %rax
        movq    %rax, (MWORD*OP_ISLE)(%rdx)
        leaq    _bci_isgt(%rip), %rax
        movq    %rax, (MWORD*OP_ISGT)(%rdx)
        leaq    _bci_iseq(%rip), %rax
        movq    %rax, (MWORD*OP_ISEQ)(%rdx)
        leaq    _bci_isne(%rip), %rax
        movq    %rax, (MWORD*OP_ISNE)(%rdx)
        leaq    _bci_ifunc(%rip), %rax
        movq    %rax, (MWORD*OP_IFUNC)(%rdx)
        leaq    _bci_call(%rip), %rax
        movq    %rax, (MWORD*OP_CALL)(%rdx)
        leaq    _bci_ret1(%rip), %rax
        movq    %rax, (MWORD*OP_RET1)(%rdx)
        ret

.globl _bci_exit
_bci_exit:
        // before returning adjust the base pointer
        //int $3
        movq    LSTATE, SAVE_L
        //movq    SAVE_L, LSTATE
        movq    BASE, OFS_L_BASE(LSTATE)
        movq    RET_TOP, OFS_L_MAXSTACK(LSTATE)
        // ... and the top pointer
        //movq    -MWORD(BASE), NODE  // RA ptr to closure
        movq    (NODE), RA  // RA = ptr to first instruction
        movq    (RA), RA
        movzx   RAH, RAd
        //int $3
        leaq    (BASE,RA,MWORD), RA
        movq    RA, OFS_L_TOP(LSTATE)
        restore_regs
        ret

        .private_extern _bci_mov_rk
_bci_mov_rk:
        ins_AD
        movq    (KBASE,RD,MWORD), RD
        movq    RD, (BASE,RA,MWORD)
        ins_next

        .private_extern _bci_mov_rr
_bci_mov_rr:
        ins_AD
        movq    (BASE,RD,MWORD), RD
        movq    RD, (BASE,RA,MWORD)
        ins_next

        .private_extern _bci_kshort
_bci_kshort:
        ins_AD
        movsx   RDW, RD
        movq    RD,(BASE,RA,MWORD)
        ins_next

	.private_extern _bci_addi_rr
_bci_addi_rr:
	// R(A) <- R(B) + R(C)
	ins_ABC
	movq    (BASE,RB,MWORD), RB
	addq	(BASE,RC,MWORD), RB
	movq	RB, (BASE,RA,MWORD)
        ins_next

        .private_extern _bci_subi_rr
_bci_subi_rr:
	// R(A) <- R(B) + R(C)
	ins_ABC
	movq    (BASE,RB,MWORD), RB
	subq	(BASE,RC,MWORD), RB
	movq	RB, (BASE,RA,MWORD)
        ins_next

        .private_extern _bci_muli_rr
_bci_muli_rr:
	// R(A) <- R(B) + R(C)
	ins_ABC
	movq    (BASE,RB,MWORD), RB
	imulq	(BASE,RC,MWORD), RB
	movq	RB, (BASE,RA,MWORD)
        ins_next

        .private_extern _bci_jmp
_bci_jmp:
	// A unused, RD = target
        ins_AJ
        branchPC RD
        ins_next

        .private_extern _bci_islt
_bci_islt: // signed integer semantics
	ins_AD
	// TODO: check arg type?
	inc_PC
	movq	(BASE,RA,MWORD), RA
	cmpq	(BASE,RD,MWORD), RA
	jge .islt
	movzxw	-2(PC), RD
	branchPC RD
.islt:
	ins_next

        .private_extern _bci_isge
_bci_isge: // signed integer semantics
	ins_AD
	inc_PC
	movq	(BASE,RA,MWORD), RA
	cmpq	(BASE,RD,MWORD), RA
	jl .isge
	movzxw	-2(PC), RD
	branchPC RD
.isge:
	ins_next

        .private_extern _bci_isle
_bci_isle: // signed integer semantics
	ins_AD
	inc_PC
	movq	(BASE,RA,MWORD), RA
	cmpq	(BASE,RD,MWORD), RA
	jg .isle
	movzxw	-2(PC), RD
	branchPC RD
.isle:
	ins_next

        .private_extern _bci_isgt
_bci_isgt: // signed integer semantics
	ins_AD
	inc_PC
	movq	(BASE,RA,MWORD), RA
	cmpq	(BASE,RD,MWORD), RA
	jle .isgt
	movzxw	-2(PC), RD
	branchPC RD
.isgt:
	ins_next

        .private_extern _bci_iseq
_bci_iseq: // signed integer semantics
	ins_AD
	inc_PC
	movq	(BASE,RA,MWORD), RA
	cmpq	(BASE,RD,MWORD), RA
	jne .iseq
	movzxw	-2(PC), RD
	branchPC RD
.iseq:
	ins_next

        .private_extern _bci_isne
_bci_isne: // signed integer semantics
	ins_AD
	inc_PC
	movq	(BASE,RA,MWORD), RA
	cmpq	(BASE,RD,MWORD), RA
	je .isne
	movzxw	-2(PC), RD
	branchPC RD
.isne:
	ins_next

        .private_extern _bci_call
_bci_call:
        // RA = new base = function pointer
        // RC = nargs + 1
        movq    (BASE,RA,MWORD), NODE      // get function pointer
        leaq    MWORD(BASE,RC,MWORD), BASE // BASE = new base
        //int $3
        // Save PC onto the PC stack
        //movq    SAVE_L, KBASE              // load L
        //movq    OFS_L_MAXSTACK(KBASE), RET_TOP // r8 = pc stack
        movq    PC, (RET_TOP)     //
        subq    $(MWORD), RET_TOP  // decrement pc stack pointer
        //movq    RET_TOP, OFS_L_MAXSTACK(KBASE)
        movq    OFS_FUNC_PC(NODE), PC     // load new PC from function
        // Dispatch first target instruction (different from ins_next!)
        movl    (PC), RAd
        movzx   RAL, OPd  // overwrites RB (OP = RB)
        movzx   RAH, RAd
        inc_PC
        jmp     *(DISPATCH,OP,MWORD)

        .private_extern _bci_ifunc
_bci_ifunc:
        // At this point we have:  (enforced by BC_CALL*)
        //   BASE = new base
        //   RA = (part of the instruction) frame size
        //   RD (= RC) = nargs + 1
        //   KBASE = &L
        //   %r8 = PC stack top
        //   PC (as usual) points to the next instruction
        //int $3
        //movq    SAVE_L, RB
        leaq    (BASE,RA,MWORD), RA  // new top of stack
        cmp     RET_TOP, RA
        jae     _vm_growstack
        movzx   (OFS_PROTO_K-OFS_PROTO_CODE-4)(PC), KBASE
        ins_next

_bci_ret1:
        // Return rA = the (single) result
        // get back return address
        //movq    SAVE_L, LSTATE              // load L
        //movq    OFS_L_MAXSTACK(LSTATE), RET_TOP // r8 = pc stack
        movq    MWORD(RET_TOP), PC
        addq    $(MWORD), RET_TOP
        //movq    RET_TOP, OFS_L_MAXSTACK(KBASE)

        // copy down result into R(0)
        movq    (BASE,RA,MWORD), RB
        movq    RB, (BASE)
        //int $3
        /*
        PC now points to the instruction that we are going to
	return to.  Thus PC-4 is the instruction that called us.
        The call instruction can tell us how big the offset between
	our current base pointer and the base pointer of the calling
	function is.  This information is given in the A field of the
	calling instruction.
        */
        movl    -4(PC), RAd
        shrl    $8, RAd
        movzx   RAL, RA
        //movzx   -3(PC), RAL
        notq    RA      // ~RA = -(RA+1)
        leaq    (BASE,RA,MWORD), BASE  // base = base - (RA+1)*MWORD

        // now reload NODE and KBASE
        movq    -MWORD(BASE), NODE    // NODE = function ptr
        movq    OFS_FUNC_PC(NODE), KBASE
        movq    (OFS_PROTO_K-OFS_PROTO_CODE-4)(KBASE), KBASE
        
        ins_next

        .globl _vm_growstack
_vm_growstack:
        // TODO
        movl    1, %eax  // encode stackoverflow
        jmp     _bci_exit

.globl  _vm_enter
_vm_enter:
        int $3
        save_regs
        movq    CARG1, DISPATCH
        movq    CARG2, KBASE
        movq    CARG3, BASE
        movq    CARG4, PC
        ins_next

.globl  _vm_call
_vm_call:
        save_regs
        movq    CARG1, LSTATE
        movq    CARG2, RD
        // CARG1, L
        movq    (LSTATE), DISPATCH
        movq    OFS_L_BASE(LSTATE), BASE
        // [BASE] = function closure
        movq    (BASE), NODE
        movq    OFS_L_MAXSTACK(LSTATE), RET_TOP
        addq    $(MWORD), BASE
        movq    (NODE), PC
        movq    (OFS_PROTO_K-OFS_PROTO_CODE)(PC), KBASE
        movq    LSTATE, SAVE_L
        ins_callt
        
        .subsections_via_symbols
