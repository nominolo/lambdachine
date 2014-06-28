#include "asm_offsets.incl"
#include "opcodes.h"

#define  MAGIC   $40

/*
x86 C calling convention
------------------------	
  
    arg3			[esp+12]
    arg2			[esp+8]
    arg1			[esp+4]
    ---- 16 byte aligned on OS X ----
    return addr			[esp]

Interpreter "calling" convention
--------------------------------

These invariants must hold after the instruction decode headers and
before the instructon dispatch footer.

  - `BASE` points to the base of the stack frame.  Registers are
    accessed relative to this pointer.  I.e.,
	
         R(x) = BASE + x * wordsize

  - `KBASE` points to the constants for the current byte code.

  - `PC` always points to the *next* instruction.

  - `DISPATCH` always points to the dispatch table.

  - `RA`, `RB`, `RC`, `RD` hold operands A, B, C, and D, respectively

All these variables should be assigned fixed machine registers.

The instruction decoding header code in addition maintains the
invariant that RA is already decoded, OP contains the current
instructions opcode, and RC holds the 16 high bits of the current
instruction.  By using the same register for RC and RD no further
decoding is needed if the instruction is of the AD format.

Even if we don't use OP, we know where it is at instruction entry.
This allows installing a generic instruction handler (e.g., for
logging or sanity checks) that still has enough information to
eventually dispatch to the real implementation (or emulate).

TODO: We might want an ALLOC pointer for bump allocation.
	
 */
#define	BASE		%edx
#define KBASE		%edi
#define PC		%esi
#define DISPATCH	%ebx

#define RA		%ecx
#define	RAH		%ch
#define RAL		%cl
#define RB		%ebp
#define RC		%eax
#define RCW		%ax
#define RCH		%ah
#define RCL		%al

#define OP		RB
#define RD		RC
#define RDW		RCW
#define RDL		RCL

#define BCBIAS_J	0x8000

	// Dispatch the next instruction.


	// Instruction decode headers for various instruction formats.
	.macro ins_A
	.endm
	
	.macro ins_AD
	.endm

	.macro ins_AJ
	.endm

	.macro ins_ABC
	movzx	RCH, RB
	movzx	RCL, RC
	.endm

	.macro ins_AB_
	movzx	RCH, RB
	.endm

	.macro ins_A_C
	movzx	RCL, RC
	.endm

	.macro ins_AND
	notl	RD
	.endm

	// Load new PC from reg (relative address)
	.macro branchPC // argument is the register to be used
	leal	-(BCBIAS_J*4)(PC,$0,4), PC
	.endm

	// Two possible implementations.  Both require 3 bytes.
	//
	// TODO: benchmark which one works best (likely to be arch-dependent)
	.macro inc_PC
	//leal	4(PC), PC
	addl	$(4), PC   // slightly (1%) faster on 2.4 GHz Core 2 Duo
	.endm

// Tail Call sequence
// BASE = new base, RB = ptr to function, RD = nargs+1, [BASE-4] = PC
	.macro ins_callt
	movl	FIELD(RB, FUNC, pc), PC
	movl 	(PC), RA
	movzx	RAL, OP
	movzx	RAH, RA
	inc_PC
	jmp	*(DISPATCH,OP,4)
	.endm

	.macro ins_next
	movl 	(PC), RC
	movzx	RCH, RA
	movzx   RCL, OP
	inc_PC
	shrl	$(16), RC
	jmp	*(DISPATCH,OP,4)
	.endm

// Stack frame layout:
//
//	[esp+60]	C arg4 / SAVE_ERRF // arguments _from_ C
//	[esp+56]	C arg3 / SAVE_NRES
//	[esp+52]	C arg2 / SAVE_CFRAME / INARG_BASE
//	[esp+48]	C arg1 / SAVE_L
//      --------- 16 byte aligned ----------
//	[esp+44]	saved return address (from C code) / SAVE_RET
//	[esp+40]	saved ebp / SAVE_R4
//	[esp+36]	saved edi / SAVE_R3 
//	[esp+32]	saved esi / SAVE_R2
//      --------- 16 byte aligned ----------
//	[esp+28]	saved ebx / SAVE_R1
//      [esp+24]	SAVE_PC
//	[esp+20]	TMP2
//	[esp+16]	TMP1
//      --------- 16 byte aligned ----------
//	[esp+12]	ARG4  // arguments _to_ C
//	[esp+8]		ARG3
//	[esp+4]		ARG2
//	[esp]		ARG1

#define CFRAME_SPACE	7*4

#define CARG1	48

	.macro save_regs
	pushl	%ebp
	pushl	%edi
	pushl	%esi
	pushl	%ebx
	subl	$(CFRAME_SPACE), %esp
	.endm

	.macro restore_regs
	addl	$(CFRAME_SPACE), %esp
	popl	%ebx
	popl	%esi
	popl	%edi
	popl	%ebp
	.endm
	
	.text
	.align 4,0x90
.globl _bci_ret
_bci_ret:
	movb    $128, %al
	movzx   %al, %eax
	ret

//	.align 4,0x90
	.private_extern _bci_addi_rr
_bci_addi_rr:
	// R(A) <- R(B) + R(C)
	ins_ABC
	movl    (BASE,RB,4),RB
	addl	(BASE,RC,4),RB
	movl	RB,(BASE,RA,4)
	ins_next
	
	.private_extern _bci_subi_rr
_bci_subi_rr:
	// R(A) <- R(B) - R(C)
	ins_ABC
	movl    (BASE,RB,4),RB
	subl	(BASE,RC,4),RB
	movl	RB,(BASE,RA,4)
	ins_next

	.private_extern _bci_muli_rr
_bci_muli_rr:
	// R(A) <- R(B) * R(C)
	ins_ABC
	movl    (BASE,RB,4),RB
	imull	(BASE,RC,4),RB
	movl	RB,(BASE,RA,4)
	ins_next

	.private_extern _bci_jmp
_bci_jmp:
	// A unused, RD = target
	ins_AJ
        // int $3
	branchPC RD
	ins_next

	.private_extern _bci_islt
_bci_islt: // signed integer semantics
	ins_AD
	// TODO: check arg type?
	inc_PC
	movl	(BASE, RA, 4), RA
	cmpl	(BASE, RD, 4), RA
	jge isltL2
	movzxw	-2(PC), RD
	branchPC RD
isltL2:
	ins_next

	.private_extern _bci_isge
_bci_isge: // signed integer semantics
	ins_AD
	// TODO: check arg type?
	inc_PC
	movl	(BASE, RA, 4), RA
	cmpl	(BASE, RD, 4), RA
	jl isgeL2
	movzxw	-2(PC), RD
	branchPC RD
isgeL2:
	ins_next

	.private_extern _bci_isle
_bci_isle: // signed integer semantics
	ins_AD
	// TODO: check arg type?
	inc_PC
	movl	(BASE, RA, 4), RA
	cmpl	(BASE, RD, 4), RA
	jg isleL2
	movzxw	-2(PC), RD
	branchPC RD
isleL2:
	ins_next

	.private_extern _bci_isgt
_bci_isgt: // signed integer semantics
	ins_AD
	// TODO: check arg type?
	inc_PC
	movl	(BASE, RA, 4), RA
	cmpl	(BASE, RD, 4), RA
	jle isgtL2
	movzxw	-2(PC), RD
	branchPC RD
isgtL2:
	ins_next

	.private_extern _bci_iseq
_bci_iseq:
	ins_AD
	// TODO: check arg type?
	inc_PC
	movl	(BASE, RB, 4), RB
	cmpl	(BASE, RD, 4), RB
	jne isgtL2
	movzxw	-2(PC), RD
	branchPC RD
iseqL2:
	ins_next

	.private_extern _bci_isne
_bci_isne:
	ins_AD
	// TODO: check arg type?
	inc_PC
	movl	(BASE, RB, 4), RB
	cmpl	(BASE, RD, 4), RB
	je isneL2
	movzxw	-2(PC), RD
	branchPC RD
isneL2:
	ins_next

        .private_extern _bci_kshort
_bci_kshort:
        ins_AD // RA = dst, RD = int16_t literal (signed)
        movsx   RDW, RD
        movl    RD, (BASE,RA,4)
        ins_next

	.private_extern _vm_init_dispatch
_vm_init_dispatch:
	// (AsmFunction *disp_tbl)
	//
	// We need to initialise the dispatch table in assembly,
	// because we're writing position-independent code.
	//
	// TODO: Use some data section which gets initialised to the
	// relative offsets, then just copy or patch this table.
	
	movl	4(%esp), %edx  // edx = ptr to dispatch table
	call Lpc1
Lpc1:
	popl	%ecx   // ecx = address of Lpc1
	leal	_vm_leave-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_EXIT)(%edx)
	leal	_bci_addi_rr-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_ADDI_RR)(%edx)
	leal	_bci_subi_rr-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_SUBI_RR)(%edx)
	leal	_bci_muli_rr-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_MULI_RR)(%edx)
	leal	_bci_kshort-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_KSHORT)(%edx)
	leal	_bci_jmp-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_JMP)(%edx)
	leal	_bci_islt-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_ISLT)(%edx)
	leal	_bci_isge-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_ISGE)(%edx)
	leal	_bci_isle-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_ISLE)(%edx)
	leal	_bci_isgt-Lpc1(%ecx), %eax
	movl	%eax, (4*OP_ISGT)(%edx)

	movl	$255, %eax // return highest index (= size of table - 1)
	ret

        .globl _vm_bench
_vm_bench:
        movl 4(%esp), %ecx
        xorl %eax, %eax
        xorl %edx, %edx
vb_loop:
        test %ecx, %ecx
        jz vb1
        addl $1, %eax
        addl $1, %eax
        addl $1, %eax
        addl $1, %eax
        subl $1, %ecx
        jmp vb_loop
vb1:
        ret

	.align 4,0x90
	.private_extern _vm_resume
_vm_resume:
	save_regs
	movl	CARG1(%esp), RA // L
	movl	OFS_GBL_STATE(RA), RB
	leal	OFS_DISPATCH_TBL(RB), DISPATCH
	movl	OFS_BASE(RA), BASE
	movl	OFS_SAVEDPC(RA), PC
	//int $3

	ins_next

	// fallthrough
	//.private_extern _vm_leave
	.globl _vm_leave
_vm_leave:
	restore_regs
	//movl	_vm_leave, %eax
 	// xorl	%eax, %eax
	ret
	.subsections_via_symbols

