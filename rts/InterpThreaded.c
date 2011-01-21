/*
 * The reference interpreter using direct threading.
 *
 * This only works with GCC because it requires GNU's "labels as
 * values" extensions.
 *
 */

#include "Common.h"
#include "Bytecode.h"
#include "InfoTables.h"
#include "Thread.h"
#include "MiscClosures.h"
#include "PrintClosure.h"

#include <stdio.h>
#include <stdlib.h>

/*********************************************************************

Stack frame layout and invariants
---------------------------------

 - The stack grows upwards.


    +----------------+
    |   register N   |
    +----------------+ <--- base[N]
    :                :
    :                :
    +----------------+
    |   register 0   |
    +----------------+ <--- base
    |      Node      | .. points to the current closure (which in
    |----------------|    turn points to the info table)
    |  return addr.  | .. points to the byte code instruction to
    |----------------|    retur to.
    | previous base  | .. a pointer (or offset) to the previous base
    +----------------+




The entry frame
---------------

A newly created stack is populated with the entry frame.  This looks
as follows:



*********************************************************************/

int engine(Thread *T);
void printStack(Word *base, Word *bottom);
void printFrame(Word *base, Word *top);


Closure *
startThread(Thread *T, Closure *cl)
{
  int ans;
  T->base[0] = (Word)cl;
  ans = engine(T);
  if (ans != 0) {
    fprintf(stderr, "ABORT: Interpreter exitited abnormally (%d)\n", ans);
    exit(1);
  }
  return (Closure*)T->stack[1];
}

#define STACK_FRAME_SIZEW   3
#define UPDATE_FRAME_SIZEW  (STACK_FRAME_SIZEW + 2)

typedef void* Inst;

void printIndent(int i, char c);

int engine(Thread* T)
{
  static Inst disp1[] = {
#define BCIMPL(name,_) &&op_##name,
    BCDEF(BCIMPL)
#undef BCIMPL
    &&stop
  };
  Inst *disp = disp1;


  Word *base = T->base;
  // The program counter always points to the *next* instruction to be
  // decoded.
  u4 *pc = T->pc;
  u4 opA, opB, opC, opcode;
  Word last_result = 0;
  Word callt_temp[BCMAX_CALL_ARGS];
  LcCode *code = NULL;

# define DBG_IND(stmt) \
  do { printIndent(base - T->stack, ' '); stmt; } while (0)
# define DBG_ENTER(info) \
  do { printIndent(base - T->stack - 1, '='); \
    printf(" ENTER %s (%p)\n", (info)->name, (info)); } while (0)
# define DBG_RETURN(info, pc) \
  DBG_IND(printf("Returning to: %s (%p), PC = %p\n", \
		 (info)->name, (info), (pc)))
# define DBG_STACK \
  do { printStack(base, T->stack); } while (0)

  /*
    At the beginning of an instruction the following holds:
    - pc points to the next instruction.
    - opcode contains the current opcode
    - opA has been decoded
    - opC is the D or SD operand
  */
# define DISPATCH_NEXT \
    opcode = bc_op(*pc); \
    DBG_IND(printf("%p %s\n", pc, ins_name[opcode]));	\
    opA = bc_a(*pc); \
    opC = bc_d(*pc); \
    ++pc; \
    goto *disp[opcode]

/* Decode the B and C operand from D. */
# define DECODE_BC \
    opB = bc_b_from_d(opC); \
    opC = bc_c_from_d(opC)
# define DECODE_AD \
    ;

  // Dispatch first instruction
  DISPATCH_NEXT;

 stop:
  T->pc = pc;
  T->base = base;
  return 0;

 op_ADDRR:
  DECODE_BC;
  base[opA] = base[opB] + base[opC];
  DISPATCH_NEXT;

 op_SUBRR:
  DECODE_BC;
  base[opA] = base[opB] - base[opC];
  DISPATCH_NEXT;

 op_MULRR:
  DECODE_BC;
  base[opA] = (WordInt)base[opB] * (WordInt)base[opC];
  DISPATCH_NEXT;

 op_DIVRR:
  DECODE_BC;
  if ((WordInt)base[opC] != 0)
    base[opA] = (WordInt)base[opB] / (WordInt)base[opC];
  else
    ; // TODO: Throw exception
  DISPATCH_NEXT;


 op_JMP:
  DECODE_AD;
  // add opC to the current pc (which points to the next instruction).
  // This means "JMP 0" is a No-op, "JMP -1" is an infinite loop.
  pc += bc_j_from_d(opC);
  DISPATCH_NEXT;

 op_MOV:
  DECODE_AD;
  base[opA] = base[opC];
  DISPATCH_NEXT;

 op_KINT:
  DECODE_AD;
  /* D = signed 16 bit integer constant */
  base[opA] = (WordInt)opC;
  DISPATCH_NEXT;

 op_NEW_INT:
  // A = result (IntClosure*)
  // C/D = value
  DECODE_AD;
  {
    WordInt val = base[opC];

    if (val >= -128 && val <= 127) {
      base[opA] = (Word)&smallInt(val);
    } else {
      IntClosure *cl = allocate(cap0, 2);
      base[opA] = (Word)cl;
      cl->info = &stg_Izh_con_info;
      cl->val = val;
    }
    DISPATCH_NEXT;
  }

 op_NOT:
  DECODE_AD;
  base[opA] = ~base[opC];
  DISPATCH_NEXT;

 op_NEG:
  DECODE_AD;
  base[opA] = -(WordInt)base[opC];
  DISPATCH_NEXT;

  /* Conditional branch instructions are followed by a JMP
     instruction, but we implement both together. */
 op_ISLT:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] < (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISGE:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] >= (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISLE:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] <= (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISGT:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] > (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISNE:
  DECODE_AD;
  ++pc;
  if (base[opA] != base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISEQ:
  DECODE_AD;
  ++pc;
  if (base[opA] == base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ALLOC1:
  // A = target
  // B = itbl
  // C = payload[0]
  {
    DECODE_BC;
    Closure *cl = malloc(sizeof(ClosureHeader) + sizeof(Word));
    setInfo(cl, (InfoTable*)base[opB]);
    cl->payload[0] = base[opC];
    base[opA] = (Word)cl;
    DISPATCH_NEXT;
  }

 op_ALLOC:
  // A = target
  // B = itbl
  // C = payload size
  // payload regs
  {
    DECODE_BC;
    u4 sz = opC;
    u4 i;
    u1 *arg = (u1 *)pc;
    Closure *cl = malloc(sizeof(ClosureHeader) + sz * sizeof(Word));
    setInfo(cl, (InfoTable*)base[opB]);
    for (i = 0; i < sz; i++)
      cl->payload[i] = base[*arg++];
    base[opA] = (Word)cl;
    pc += (sz + 3) / sizeof(BCIns);
    DISPATCH_NEXT;
  }

 op_LOADF:
  // A = target
  // B = closure ptr.
  // C = closure offset
  {
    DECODE_BC;
    u4 offset = (u1)opC;
    Closure *cl = (Closure*)base[opB];
    base[opA] = cl->payload[offset - 1];
    DISPATCH_NEXT;
  }

 op_LOADFV:
  // A = target
  // C/D = offset
  {
    u4 offset = (u2)opC;
    Closure *node = (Closure*)base[-1];
    base[opA] = node->payload[offset - 1];
    DISPATCH_NEXT;
  }

 op_LOADBH:
  // A = target
  {
    base[opA] = (Word)&stg_BLACKHOLE_closure;
    DISPATCH_NEXT;
  }

 op_LOADSLF:
  // A = target
  {
    base[opA] = base[-1];
    DISPATCH_NEXT;
  }

 op_JFUNC:
 op_IFUNC:
 op_FUNC:
  // ignore
  DISPATCH_NEXT;

 op_CASE:
  // A case with compact targets.
  //
  //  +-----------+-----+-----+
  //  | num_cases |  A  | OPC |
  //  +-----------+-----+-----+
  //  | target_1  | target_0  |  target_i:
  //  +-----------+-----------+    goto this address if tag = i
  //  :                       :
  //  +-----------+-----------+  targetN may be 0 if num_cases is odd.
  //  | target_N  | target_N-1|
  //  +-----------+-----------+
  //  :  default case follows :
  //  +- - - - - - - - - - - -+
  //
  // Targets are non-negative numbers.  They are interpreted as
  // offsets relative to the _end_ of the instruction.  That is "0"
  // denotes the instruction directly following the CASE instruction.
  //
  // If num_cases is smaller than the tag, then we just fall through
  // to the default case.
  //
  // A = thing to dispatch on (must be a constructor node)
  // D = number of cases
  //
  {
    Closure *cl = (Closure *)base[opA];
    u2 num_cases = opC;
    BCIns *table = pc;
    pc += (num_cases + 1) >> 1;
    // assert cl->info.type == CONSTR

    u2 tag = getTag(cl) - 1;  // tags start at 1
    //printf("CASE, tag = %d\n", tag);

    if (tag < num_cases) {
      BCIns target = table[tag >> 1];
      u2 offs =
        tag & 1 ? bc_case_target(target) : bc_case_targetlo(target);
      pc += offs;
    }

    DISPATCH_NEXT;
  }

 op_CASE_S:
  // Sparse CASE.  A case with possibly missing tags.
  //
  //  +-----------+-----+-----+
  //  | num_cases |  A  | OPC |
  //  +-----------+-----+-----+
  //  | max_tag   |  min_tag  |
  //  +-----------+-----------+
  //  | target    |    tag    |  x num_cases
  //  +-----------+-----------+
  //  :  default case follows :
  //  +- - - - - - - - - - - -+
  //
  // The (tag, target) items must be in ascending order.  This allows us
  // to use binary search to find the matching case.
  //
  {
    Closure *cl = (Closure*)base[opA];
    u2 num_cases = opC;
    u2 min_tag = bc_case_mintag(*pc);
    u2 max_tag = bc_case_maxtag(*pc);
    BCIns *table = pc + 1;
    pc += 1 + num_cases;

    LC_ASSERT(cl != NULL && getInfo(cl)->type == CONSTR);
    u2 tag = getTag(cl);
    int istart = 0;
    int ilen = num_cases;
    int imid = 0;

    if (tag >= min_tag && tag <= max_tag) {
      // Use binary search if there's more than 4 entries
      while (ilen > 4) {
        int imid = (istart + istart + ilen) / 2;
        if (bc_case_tag(table[imid]) == tag)
          goto op_CASE_S_found;
        else if (bc_case_tag(table[imid]) < tag)
          ilen = imid - istart;
        else { // > tag
          ilen = istart + ilen + 1 - imid;
          istart = imid + 1;
        }
      }

      // The linear search for up to 4 entries
      for (imid = istart; ilen > 0; ilen--, imid++)
        if (bc_case_tag(table[imid]) == tag)
          goto op_CASE_S_found;

    }
    // nothing found
    DISPATCH_NEXT;

  op_CASE_S_found:
    LC_ASSERT(bc_case_tag(table[imid]) == tag);
    pc += bc_case_target(table[imid]);
    DISPATCH_NEXT;
  }

 op_EVAL:
  // Format of an EVAL instruction:
  //
  //  +-----------+-----+-----+
  //  |     -     |  A  | OPC |
  //  +-----------+-----+-----+
  //  |   live-outs bitmask   |
  //  +-----------+-----------+
  //
  {
    // opA = thing to evaluate
    Closure *tnode = (Closure *)base[opA];

    LC_ASSERT(tnode != NULL);
    LC_ASSERT(getInfo(tnode) != NULL);

    DBG_IND(printf("evaluating: %p\n", tnode));
    DBG_IND(printf("itbl %p\n", getFInfo(tnode)->name));

    while (closure_IND(tnode)) {
      DBG_IND(printf("... following indirection\n"));
      tnode = (Closure*)tnode->payload[0];
    }

    if (closure_HNF(tnode)) {
      DBG_IND(printf("         (in HNF)\n"));
      last_result = (Word)tnode;
      pc += 1; // skip live-out info
      DISPATCH_NEXT;
    } else {
      Word *top = T->top; //base + node->info->code.framesize;
      ThunkInfoTable *info = (ThunkInfoTable*)getInfo(tnode);

      u4 framesize = info->code.framesize;
      DBG_ENTER(info);
      DBG_STACK;

      if (stackOverflow(T, T->top, STACK_FRAME_SIZEW + UPDATE_FRAME_SIZEW +
                        framesize)) {
        printf("Stack overflow.  TODO: Automatically grow stack.\n");
        return -1;
      }

      BCIns *return_pc = pc + 1; // skip live-out info
      // push update frame and enter thunk
      top[0] = (Word)base;
      top[1] = (Word)return_pc;
      top[2] = (Word)&stg_UPD_closure;
      top[3] = (Word)tnode; // reg0
      top[4] = 0;           // reg1
      top[5] = (Word)&top[3];
      top[6] = (Word)stg_UPD_return_pc;
      top[7] = (Word)tnode;

      base = top + STACK_FRAME_SIZEW + UPDATE_FRAME_SIZEW;
      T->top = base + framesize;
      code = &info->code;
      pc = info->code.code;
      DISPATCH_NEXT;
    }

  }

 op_UPDATE:
  // opC/D = new value
  // opA = old value
  //
  // Make old_value point to new_value by overwriting the closure for
  // old_value with an indirection to new_value.  Then return new_value.
  {
    Closure *oldnode = (Closure *)base[opA];
    Closure *newnode = (Closure *)base[opC];
    DBG_IND(printf("... updating: %p with %p\n", oldnode, newnode));
    setInfo(oldnode, (InfoTable*)&stg_IND_info);
    // TODO: Enforce invariant: *newcode is never an indirection.
    oldnode->payload[0] = (Word)newnode;
    last_result = (Word)newnode;
    goto do_return;
  }

 op_RET1:
  // opA = result
  //
  // The return address is on the stack. just jump to it.
  last_result = base[opA];
 do_return:
  T->top = base - 3;
  pc = (BCIns*)base[-2];
  base = (Word*)base[-3];
  { FuncInfoTable *info = getFInfo((Closure*)base[-1]);
    DBG_RETURN(info, pc);
    code = &info->code;
  }
  DISPATCH_NEXT;

 op_MOV_RES:
  // Copy last function call result into a register.
  //
  // opA = target register
  base[opA] = last_result;
  DISPATCH_NEXT;

 op_CALLT:
  {
    // opA = function
    // opB = no of args
    // opC = first argument
    DECODE_BC;
    u4 nargs = opB;
    Word arg0 = base[opC];
    Closure *fnode = (Closure *)base[opA];
    u1 *args = (u1*)pc;
    u4 i;

    LC_ASSERT(fnode != NULL);

    if (nargs > BCMAX_CALL_ARGS) {
      printf("Too many arguments to CALLT.  (Bug in code gen?)\n");
      return -1;
    }

    u4 arg_offs = 0;
    FuncInfoTable *info;
    switch (getInfo(fnode)->type) {
    case PAP:
      {
	PapClosure *pap = (PapClosure*)fnode;
	fnode = pap->fun;
	LC_ASSERT(getInfo(fnode)->type == FUN);
	info = getFInfo(fnode);

	DBG_IND(printf("calling a PAP%d (%s)\n", pap->nargs, info->name));

	// TODO: special case for another partial application?
	arg_offs = pap->nargs;
	for (i = 0; i < arg_offs; i++) {
	  callt_temp[i] = (Word)pap->payload[i];
	}
	nargs += pap->nargs;
      }
      break;
    case FUN:
      info = getFInfo(fnode);
      break;
    default:
      // TODO: should we do an automatic EVAL here?
      fprintf(stderr, "FATAL: Function argument to CALLT not FUN or PAP.\n");
      exit(1);
    }
    // Because we do not yet require to allocate CALLT arguments in
    // registers r0, r1, ..., we need to copy all of them into a scratch
    // area (to avoid overwriting one value with another).
    callt_temp[arg_offs] = arg0;
    LC_ASSERT(LC_ARCH_ENDIAN == LAMBDACHINE_LE); // XXX: generalise
    for (i = arg_offs + 1; i < nargs; i++, args++) {
      callt_temp[i] = base[*args];
    }

    // At this point we have the invariants:
    //   - fnode is a pointer to a FUN
    //   - info is its info table
    //   - nargs is the *total* numbers of arguments applied (not just
    //      from this instruction)
    //   - callt_temp contains the values of all arguments in order

    if (nargs < info->code.arity) { // Partial application
      PapClosure *pap = malloc(sizeof(PapClosure) + sizeof(Word) * nargs);
      setInfo(pap, (InfoTable*)&stg_PAP_info);
      pap->arity = info->code.arity - nargs;
      pap->nargs = nargs;
      pap->fun = fnode;

      DBG_IND(printf("Creating PAP = %s, nargs = %d, arity = %d\n",
		     info->name, pap->nargs, pap->arity));

      for (i = 0; i < nargs; i++) {
	pap->payload[i] = callt_temp[i];
      }

      // return pointer to pap
      last_result = (Word)pap;
      T->top = base - 3;
      pc = (BCIns*)base[-2];
      base = (Word*)base[-3];
      { FuncInfoTable *info = getFInfo((Closure*)base[-1]);
	DBG_RETURN(info, pc);
	code = &info->code;
      }
      DISPATCH_NEXT;
    }

    if (nargs > info->code.arity) {
      // Overapplication.  See [Memo 1] for details.
      u4 immediate_args = info->code.arity;
      u4 extra_args = nargs - immediate_args;

      DBG_IND(printf(" ... overapplication: %d + %d\n",
		     immediate_args, extra_args));
      DBG_ENTER(info);

      // Change current frame
      Word *top = base + extra_args + 1;
      for (i = 0; i < extra_args; i++) {
	base[i] = callt_temp[immediate_args + i];
      }

      //printFrame(base, top);
      BCIns *ap_return_pc;
      Closure *ap_closure;
      getAPKClosure(&ap_closure, &ap_return_pc, extra_args);

      base[-1] = (Word)ap_closure;

      u4 framesize = info->code.framesize;
      if (stackOverflow(T, top, STACK_FRAME_SIZEW + framesize)) {
	printf("Stack overflow.  TODO: Automatically grow stack.\n");
	return -1;
      }

      // Build stack frame for fnode
      // TODO: Check for stack overflow.
      top[0] = (Word)base;
      top[1] = (Word)ap_return_pc;
      top[2] = (Word)fnode;
      base = top + 3;
      for (i = 0; i < immediate_args; i++) {
	base[i] = callt_temp[i];
      }
      T->top = base + framesize;
      code = &info->code;
      pc = info->code.code;
      printStack(base, T->stack);
      DISPATCH_NEXT;

    } else { // Exact application
      u4 curframesize = T->top - base;
      u4 newframesize = info->code.framesize;

      DBG_ENTER(info);

      if (newframesize > curframesize) {
        if (stackOverflow(T, base, newframesize)) {
          printf("Stack overflow.  TODO: Automatically grow stack.\n");
          return -1;
        } else {
          T->top = base + newframesize;
        }
      }

      for (i = 0; i < nargs; i++) {
	base[i] = callt_temp[i];
      }

      base[-1] = (Word)fnode;
      code = &info->code;
      pc = info->code.code;
      printStack(base, T->stack);
      DISPATCH_NEXT;
    }
  }

 op_CALL:
  {
    // opA = function
    // opB = no of args
    // opC = first argument reg
    // following bytes: argument regs, live regs
    DECODE_BC;
    // Arguments from this call instruction
    u4       callargs = opB;
    // Total number of arguments, including PAP arguments.
    u4       nargs = callargs;
    Closure *fnode = (Closure *)base[opA];
    Word     arg0  = base[opC];
    Word    *top   = T->top;
    u4 i;

    LC_ASSERT(fnode != NULL);

    FuncInfoTable *info;
    PapClosure *pap = NULL;
    switch (getInfo(fnode)->type) {
    case PAP:
      {
        pap     = (PapClosure*)fnode;
        fnode   = pap->fun;
        LC_ASSERT(getInfo(fnode)->type == FUN);
        info    = getFInfo(fnode);
        nargs  += pap->nargs;
      }
      break;
    case FUN:
      info = getFInfo(fnode);
      break;
    default:
      fprintf(stderr, "ERROR: CALL function argument not a PAP or FUN.\n");
      exit(1);
    }

    if (nargs < info->code.arity) {
      // Partial application
      //
      // Construct a PAP and return it.

      // If there is an existing PAP we do not reuse it, but instead
      // allocate a new PAP and copy over the old args and the args
      // from this call.

      PapClosure *new_pap = malloc(sizeof(PapClosure) + nargs * sizeof(Word));
      setInfo(new_pap, (InfoTable*)&stg_PAP_info);
      new_pap->arity = info->code.arity - nargs;
      new_pap->nargs = nargs;
      new_pap->fun   = fnode;

      DBG_IND(printf("Creating PAP = %s, nargs = %d, arity = %d\n",
		     info->name, pap->nargs, new_pap->arity));

      if (pap != NULL) {
        // Copy first few args from old PAP
        for (i = 0; i < pap->nargs; i++)
          new_pap->payload[i] = pap->payload[i];

        // Copy rest
        u1 *args = (u1*)pc;
        LC_ASSERT(LC_ARCH_ENDIAN == LAMBDACHINE_LE);
        new_pap->payload[pap->nargs] = arg0;
        for (i = pap->nargs + 1; i < nargs; i++, args++)
          new_pap->payload[i] = base[*args];
      }

      // Return the PAP
      last_result = (Word)new_pap;
      T->top = base - 3;
      pc     = (BCIns*)base[-2];
      base   = (Word*)base[-3];
      { FuncInfoTable *info = getFInfo((Closure*)base[-1]);
	DBG_RETURN(info, pc);
	code = &info->code;
      }
      DISPATCH_NEXT;
    }

    DBG_ENTER(info);
    DBG_STACK;

    // each additional argument requires 1 byte,
    // we pad to multiples of an instruction
    // the liveness mask follows (one instruction)
    BCIns *return_pc = pc + BC_ROUND(nargs - 1) + 1;
    u4     framesize = info->code.framesize;
    Word  *saved_base;

    if (nargs > info->code.arity) {
      // Overapplication.
      //
      // In this case we create an application stack frame below the
      // function's frame.  I.e., when the function returns, the
      // remaining arguments will be applied.

      u4 immediate_args = info->code.arity;
      u4 extra_args = nargs - immediate_args;

      u4 ap_frame_size = STACK_FRAME_SIZEW + extra_args + 1;
      if (stackOverflow(T, top, STACK_FRAME_SIZEW + framesize + ap_frame_size)) {
	fprintf(stderr, "ABORT: Stack overflow.  TODO: Automatically grow stack.\n");
	return -1;
      }

      top[0] = (Word)base;
      top[1] = (Word)return_pc;
      Closure *ap_closure;
      // Note the modification of `return_pc`.
      getAPKClosure(&ap_closure, &return_pc, extra_args);
      top[2] = (Word)ap_closure;
      saved_base = &top[3];

      u1 *args = (u1*)pc;
      args += immediate_args - 1;
      Word *p = &top[3];
      for (i = immediate_args; i < callargs; i++, p++, args++) {
        //DBG_IND(printf(" copying %d r%d %" FMT_WordX "\n", i, *args, base[*args]));
        *p = base[*args];
      }
      // Move `top`, so the code below allocates on top of it.
      top += ap_frame_size;
      // Fall through to exact arity case

    } else {

      // Exact application.
      if (stackOverflow(T, top, STACK_FRAME_SIZEW + framesize)) {
	printf("Stack overflow.  TODO: Automatically grow stack.\n");
	return -1;
      }
      saved_base = base;
    }

    top[0] = (Word)saved_base;
    top[1] = (Word)return_pc;
    top[2] = (Word)fnode;

    u4 arg0pos = 3; // index where arg0 should go
    if (pap != NULL) {
      for (i = 0; i < pap->nargs; i++) {
        top[i + 3] = pap->payload[i];
      }
      arg0pos += pap->nargs;
    }

    // copy arguments
    top[arg0pos] = arg0;
    u1 *arg = (u1*)pc;
    for (i = 1; i < callargs; i++, arg++) {
      top[arg0pos + i] = base[*arg];
    }

    base = top + STACK_FRAME_SIZEW;
    T->top = base + framesize;
    code = &info->code;
    pc = info->code.code;
    DISPATCH_NEXT;
  }

 op_LOADK:
  {
    //printf("fetching lit: r%d = lit[%d]\n", opA, opC);
    u2 lit_id = opC;
    //printf("code = %p, lits = %d\n", code, code->sizelits);
    base[opA] = code->lits[lit_id];
    DISPATCH_NEXT;
  }

 op_ALLOCAP:
  {
    DECODE_BC;
    // A = result register
    // C = number of arguments (including function), always > 1
    // B = first argument (function closure)
    u4 nargs = opC;
    u4 i;

    Closure *cl = malloc(sizeof(ClosureHeader) + (nargs + 1) * sizeof(Word));
    InfoTable *info = getAPInfoTable(nargs);
    setInfo(cl, info);

    cl->payload[0] = base[opB];
    u1 *args = (u1 *)pc;
    pc += BC_ROUND(nargs - 1);
    for (i = 0; i < nargs; i++, args++)
      cl->payload[i + 1] = base[*args];

    base[opA] = (Word)cl;
    DISPATCH_NEXT;
  }

 op_INITF:
  fprintf(stderr, "Unimplemented bytecode\n.");
  return -1;
}

static BCIns test_code[] = {
  BCINS_ABC(BC_ADDRR, 1, 0, 1),
  BCINS_ABC(BC_ADDRR, 1, 0, 1),
  BCINS_AJ(BC_JMP, 0, +1), // skip next instr.
  BCINS_ABC(BC_ADDRR, 1, 0, 1),
  BCINS_AD(BC__MAX, 0, 0) };

// static BCIns

static BCIns silly1_code[] = {
  BCINS_AD(BC_KINT, 0, 42),   // r0 = 42
  BCINS_AD(BC_NEW_INT, 0, 0), // r0 = new(I#, r0)
  BCINS_AD(BC_RET1, 0, 0)     // return r0
};

static ThunkInfoTable silly1_info = {
  .i = DEF_INFO_TABLE(THUNK, 0, 0, 1),
  .name = "silly1",
  .code = {
    .lits = NULL, .sizelits = 0, 
    .littypes = NULL,
    .code = silly1_code, .sizecode = countof(silly1_code),
    .framesize = 1, .arity = 0
  }
};

static Closure silly1 = 
  DEF_CLOSURE(&silly1_info, { 0 });
/*
int main(int argc, char* argv[])
{
  initVM();
  Thread *T0 = createThread(cap0, 1024);

  T0->base[0] = (Word)&silly1; // smallInt(0);
  //printClosure((Closure*)T0->base[0]);

  engine(T0);

  printClosure((Closure*)T0->stack[1]);
  //printf("%0" FMT_WordLen FMT_WordX "\n", T0->stack[1]);
  return 0;
}
*/
int
stackOverflow(Thread* thread, Word* top, u4 increment)
{
  return 0;
}

void
printStack(Word *base, Word *bottom)
{
  printf(">>> Stack = ");
  while (base > bottom + 1) {
    FuncInfoTable *i = getFInfo((Closure*)base[-1]);
    printf("%s : ", i->name);
    base = (Word*)base[-3];
  }
  printf("[]\n");
}

void
printFrame(Word *base, Word *top)
{
  printf("[%p]", base);
  while (base < top) {
    printf(" %" FMT_WordX, *base);
    ++base;
  }
  printf("\n");
}

void printIndent(int i, char c)
{
  while (i-- > 0) {
    putchar(c);
  }
}
