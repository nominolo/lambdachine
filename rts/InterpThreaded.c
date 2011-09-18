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
#include "StorageManager.h"
#include "Jit.h"
#include "Stats.h"
#if LC_HAS_ASM_BACKEND
#include "InterpAsm.h"
#endif

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

int engine(Capability *);
void printStack(FILE *, Word *base, Word *bottom);
void printFrame(FILE *, Word *base, Word *top);
void invalidateDeadSlots(Word *from, Word *to, BCIns *pc);

enum {
  INTERP_OK = 0,
  INTERP_OUT_OF_STEPS = 1,
  INTERP_STACK_OVERFLOW = 2,
  INTERP_UNIMPLEMENTED = 3
} InterpExitCode;

Closure *
startThread(Thread *T, Closure *cl)
{
  Capability *cap = G_cap0;
  cap->T = T;
  int ans;
  T->base[0] = (Word)cl;
  ans = engine(cap);
  if (ans != INTERP_OK) {
    fprintf(stderr, "*** ABORT: Interpreter exitited abnormally.\n"
            "***   Reason: ");
    switch (ans) {
    case INTERP_OUT_OF_STEPS:
      fprintf(stderr, "Out of steps\n"); break;
    case INTERP_STACK_OVERFLOW:
      fprintf(stderr, "Stack overflow\n"); break;
    case INTERP_UNIMPLEMENTED:
      fprintf(stderr, "Unimplemented bytecode.\n"); break;
    default:
      fprintf(stderr, "Unknown reason (%d)\n", ans);
    }
    exit(1);
  }
  return (Closure*)T->stack[3];
}

#define STACK_FRAME_SIZEW   3
#define UPDATE_FRAME_SIZEW  (STACK_FRAME_SIZEW + 2)

typedef void* Inst;

void printIndent(FILE *stream, int i, char c);

int engine(Capability *cap)
{
  Thread *T = cap->T;
  int maxsteps = 100000000;
  static Inst disp1[] = {
#define BCIMPL(name,_) &&op_##name,
    BCDEF(BCIMPL)
#undef BCIMPL
    &&stop
  };
  Inst *disp = disp1;

  Inst *disp_record;
#if LC_HAS_JIT
  JitState *J = &cap->J;
  //  initJitState(J);
  {
    int i;
    disp_record = xmalloc(sizeof(disp1));
    for (i = 0; i < countof(disp1); i++)
      disp_record[i] = &&recording;
  }

  if (cap->flags & CF_NO_JIT) {
    // Disable the JIT by overriding the places where a new trace may
    // be started.
    disp1[BC_FUNC] = disp1[BC_IFUNC];
  }

#endif
  Word *base = T->base;
  // The program counter always points to the *next* instruction to be
  // decoded.
  u4 *pc = T->pc;
  u4 opA, opB, opC, opcode;
  T->last_result = 0;
  Word callt_temp[BCMAX_CALL_ARGS];
  LcCode *code = NULL;

#if (LC_DEBUG_LEVEL >= 1)
# define DBG_IND(stmt) \
  do { printIndent(stderr, base - T->stack, '.'); stmt;} while (0)
# define DBG_ENTER(info) \
  do { printIndent(stderr, base - T->stack - 1, '=');  \
    fprintf(stderr, " ENTER %s (%p)\n", (info)->name, (info));   \
    printInfoTable(stderr, (InfoTable*)(info));                   \
    /* printFrame(base, T->top); */ } while (0)
# define DBG_RETURN(info, pc) \
  DBG_IND(fprintf(stderr, "Returning to: %s (%p), PC = %p\n",    \
		 (info)->name, (info), (pc)); \
          printInfoTable(stderr, (InfoTable*)(info)))
# define DBG_STACK \
  do { printStack(stderr, base, T->stack); } while (0)
#else
# define DBG_IND(stmt)
# define DBG_ENTER(info)
# define DBG_RETURN(info, pc)
# define DBG_STACK
#endif
  /*
    At the beginning of an instruction the following holds:
    - pc points to the next instruction.
    - opcode contains the current opcode
    - opA has been decoded
    - opC is the D or SD operand
  */
# define DISPATCH_NEXT \
    opcode = bc_op(*pc); \
    if (LC_DEBUG_LEVEL >= 2) { \
      fprintf(stderr, COL_YELLOW);                                       \
      DBG_IND(fprintf(stderr, "    "); printFrame(stderr, base, T->top)); \
      DBG_IND(printInstructionOneLine(stderr, pc));         \
      fprintf(stderr, COL_RESET);                            \
    } \
    maxsteps--;  if (maxsteps == 0) return INTERP_OUT_OF_STEPS; \
    recordEvent(EV_DISPATCH, 0); \
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

#if LC_HAS_JIT
 recording:
  //printf("%p, %p\n", pc, J->startpc);
  {
    recordEvent(EV_RECORD, 0);
    T->base = base;
    J->pc = T->pc = pc - 1;
    J->func = getFInfo((Closure*)base[-1]);
    u4 recstatus = recordIns(J);
    if (recstatus != REC_CONT) {
      //printf(COL_RED "Recording finished: %x\n" COL_RESET, recstatus);
      fprintf(stderr, "Recording finished: %x\n", recstatus);
      disp = disp1;
      switch (recstatus & REC_MASK) {
      case REC_ABORT:
      case REC_DONE:
        break; // Do Nothing, just continue interpreting
      case REC_LOOP:
        // We found a loop and want to immediately execute it.
        {
          Fragment *F = J->fragment[getFragmentId(recstatus)];
          Closure *cl;
          LC_ASSERT(F != NULL);
          recordEvent(EV_TRACE, 0);
#if LC_HAS_ASM_BACKEND
          if(J->param[JIT_P_enableasm]) {
            asmEngine(cap, F);
          }
          else {
            irEngine(cap, F);
          }
#else
	  irEngine(cap, F);
#endif
          DBG_PR("*** Continuing at: pc = %p, base = %p\n",
                 T->pc, T->base);
          //LC_ASSERT(0);
          pc = T->pc;
          base = T->base;
          cl = (Closure*)base[-1];
          code = &getFInfo(cl)->code;
          DISPATCH_NEXT;
        }
      }
    }
    // Continue interpreting
    goto *disp1[opcode];
  }
#endif
 stop:
  T->pc = pc;
  T->base = base;
  fprintf(stderr, ">>> Steps Left: %d\n", maxsteps);
  return INTERP_OK;

 op_ADDRR:
  DECODE_BC;
  recordEvent(EV_ALU, 0);
  base[opA] = base[opB] + base[opC];
  DISPATCH_NEXT;

 op_SUBRR:
  DECODE_BC;
  recordEvent(EV_ALU, 0);
  base[opA] = base[opB] - base[opC];
  DISPATCH_NEXT;

 op_MULRR:
  DECODE_BC;
  recordEvent(EV_MUL, 0);
  base[opA] = (WordInt)base[opB] * (WordInt)base[opC];
  DISPATCH_NEXT;

 op_DIVRR:
  DECODE_BC;
  recordEvent(EV_REMDIV, 0);
  if (LC_LIKELY((WordInt)base[opC] != 0))
    base[opA] = (WordInt)base[opB] / (WordInt)base[opC];
  else
    LC_ASSERT(0); // TODO: Throw exception
  DISPATCH_NEXT;

 op_REMRR:
  DECODE_BC;
  recordEvent(EV_REMDIV, 0);
  if (LC_LIKELY((WordInt)base[opC] != 0))
    base[opA] = (WordInt)base[opB] % (WordInt)base[opC];
  else
    LC_ASSERT(0);
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
      IntClosure *cl = allocClosure(wordsof(IntClosure));
      base[opA] = (Word)cl;
      cl->info = &stg_Izh_con_info;
      cl->val = val;
    }
    DISPATCH_NEXT;
  }

 op_NOT:
  DECODE_AD;
  recordEvent(EV_ALU, 0);
  base[opA] = ~base[opC];
  DISPATCH_NEXT;

 op_NEG:
  DECODE_AD;
  recordEvent(EV_ALU, 0);
  base[opA] = -(WordInt)base[opC];
  DISPATCH_NEXT;

  /* Conditional branch instructions are followed by a JMP
     instruction, but we implement both together. */
 op_ISLT:
  DECODE_AD;
  recordEvent(EV_CMP, 0);
  ++pc;
  if ((WordInt)base[opA] < (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISGE:
  DECODE_AD;
  recordEvent(EV_CMP, 0);
  ++pc;
  if ((WordInt)base[opA] >= (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISLE:
  DECODE_AD;
  recordEvent(EV_CMP, 0);
  ++pc;
  if ((WordInt)base[opA] <= (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISGT:
  DECODE_AD;
  recordEvent(EV_CMP, 0);
  ++pc;
  if ((WordInt)base[opA] > (WordInt)base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISNE:
  DECODE_AD;
  recordEvent(EV_CMP, 0);
  ++pc;
  if (base[opA] != base[opC])
    pc += bc_j(*(pc - 1));
  DISPATCH_NEXT;

 op_ISEQ:
  DECODE_AD;
  recordEvent(EV_CMP, 0);
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
    DBG_PR("ALLOC%d\n", 1);
    recordEvent(EV_ALLOC, 2);
    Closure *cl = allocClosure_(wordsof(ClosureHeader) + 1, T, pc + 1, base);
    setInfo(cl, (InfoTable*)base[opB]);
    cl->payload[0] = base[opC];
    base[opA] = (Word)cl;
    ++pc;
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
    DBG_PR("ALLOC_%d  ; ", sz);
    IF_DBG(
           if (sz == 0) {
             printInfoTable(stderr, (InfoTable*)base[opB]);
           } else { fprintf(stderr, "\n");
           });
    recordEvent(EV_ALLOC, 1 + sz);
    u4 i;
    u1 *arg = (u1 *)pc;
    BCIns *new_pc = pc + 1 + ((sz + 3) / sizeof(BCIns));
    Closure *cl = allocClosure_(wordsof(ClosureHeader) + sz, T, new_pc, base);
    setInfo(cl, (InfoTable*)base[opB]);
    for (i = 0; i < sz; i++)
      cl->payload[i] = base[*arg++];
    base[opA] = (Word)cl;
    pc = new_pc;
    DISPATCH_NEXT;
  }

 op_LOADF:
  // A = target
  // B = closure ptr.
  // C = closure offset
  {
    DECODE_BC;
    recordEvent(EV_LOAD, 0);
    u4 offset = (u1)opC;
    Closure *cl = (Closure*)base[opB];
    base[opA] = cl->payload[offset - 1];
    DISPATCH_NEXT;
  }

 op_LOADFV:
  // A = target
  // C/D = offset
  {
    recordEvent(EV_LOAD, 0);
    u4 offset = (u2)opC;
    Closure *node = (Closure*)base[-1];
    LC_ASSERT(looksLikeClosure(node));
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

#if LC_HAS_JIT
  {
    u4 frag_id = opC;
    Fragment *F = J->fragment[frag_id];
    Closure *cl;

    // Make sure thread data is consistent.
    T->base = base;
    T->pc = pc - 1;

    fprintf(stderr, "re-entering trace: %d\n", frag_id);

    LC_ASSERT(F != NULL);
    recordEvent(EV_TRACE, 0);
#if LC_HAS_ASM_BACKEND
    if(J->param[JIT_P_enableasm]) {
      asmEngine(cap, F);
    }
    else {
      irEngine(cap, F);
    }
#else
      irEngine(cap, F);
#endif
    DBG_PR("*** Continuing at: pc = %p, base = %p\n", T->pc, T->base);

    pc = T->pc;
    base = T->base;
    cl = (Closure*)base[-1];
    code = &getFInfo(cl)->code;
    DISPATCH_NEXT;
  }
#else
  DISPATCH_NEXT;
#endif

 op_FUNC:
#if LC_HAS_JIT
  recordEvent(EV_TICK, 0);
  {
    // Ignore if we're already recording.
    if (LC_UNLIKELY(J->mode != 0)) {
      DISPATCH_NEXT;
    }

    // Decrement hot counter for this PC
    //
    // We take the PC of this instruction to be the start of trace,
    // but we start recording with the next instruction.  This is fine
    // since from the trace recorder's point of view FUNC is a NOP.
    //
    HotCount c = --cap->hotcount[hotcount_hash(pc-1)];
    DBG_PR("HOT_TICK: [%d] = %d\n", hotcount_hash(pc-1), c);
    if (LC_UNLIKELY(c == 0)) {   // Target has become hot.
      hotcount_set(cap, pc-1, HOTCOUNT_DEFAULT); // Reset hotcount
      startRecording(J, pc-1, cap->T, base);
      disp = disp_record;
    }
  }
#endif
  DISPATCH_NEXT;
    
 op_IFUNC:
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
    recordEvent(EV_CASE, num_cases);
    BCIns *table = pc;
    pc += (num_cases + 1) >> 1;
    LC_ASSERT(getInfo(cl)->type == CONSTR);

    u2 tag = getTag(cl) - 1;  // tags start at 1
    DBG_IND(fprintf(stderr, "... tag = %d\n", tag + 1));

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
    recordEvent(EV_CASE, num_cases);
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
    LC_ASSERT(!looksLikeInfoTable((void*)tnode));
    LC_ASSERT(getInfo(tnode) != NULL);

    DBG_IND(fprintf(stderr, "evaluating: %p\n", tnode));
    DBG_IND(fprintf(stderr, "closure: ");printClosure(tnode);fprintf(stderr, "\n"););
    DBG_IND(fprintf(stderr, "itbl %p\n", getFInfo(tnode)->name));

    while (closure_IND(tnode)) {
      DBG_IND(fprintf(stderr, "... following indirection\n"));
      tnode = (Closure*)tnode->payload[0];
    }

    if (closure_HNF(tnode)) {
      recordEvent(EV_EVAL_HNF, 0);
      DBG_IND(fprintf(stderr, "         (in HNF)\n"));
      T->last_result = (Word)tnode;
      pc += 1; // skip live-out info
      DISPATCH_NEXT;
    } else {
      Word *top = T->top; //base + node->info->code.framesize;
      ThunkInfoTable *info = (ThunkInfoTable*)getInfo(tnode);
      recordEvent(EV_EVAL_THUNK, 0);

      // NOTE: At this poin we would normally overwrite the info table
      // of the thunk with a BLACKHOLE (or more precisely we overwrite
      // the info table).  However, we cannot do this due to the
      // literal table.  Consider this sequence of events:
      //
      //  1. We enter a thunk and overwrite it with a BLACKHOLE
      //  2. The evaluation code calls some function.
      //  3. The function returns and reloads the literal table
      //     from the saved Node pointer which now points to the
      //     BLACKHOLE info table which does not have any literals.
      //  4. The evaluation code tries to load a literal.
      //
      // We now get an invalid memory access.

      u4 framesize = info->code.framesize;
      DBG_ENTER(info);
      DBG_STACK;

      if (stackOverflow(T, T->top, STACK_FRAME_SIZEW + UPDATE_FRAME_SIZEW +
                        framesize)) {
        return INTERP_STACK_OVERFLOW;
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

      IF_DBG_LVL(0, invalidateDeadSlots(base, top, return_pc));

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
    const InfoTable *info = getInfo(oldnode);
    LC_ASSERT(newnode != 0);
    recordEvent(EV_UPDATE, 0);

    DBG_IND(fprintf(stderr, "... updating: %p with %p\n", oldnode, newnode));
    DBG_IND(fprintf(stderr, "...  closure: "); printClosure(newnode);fprintf(stderr, "\n"););
    setInfo(oldnode, (InfoTable*)&stg_IND_info);
    // TODO: Enforce invariant: *newnode is never an indirection.
    DBG_IND(fprintf(stderr, "... writing to mem loc: %p\n", &oldnode->payload[0]));
    oldnode->payload[0] = (Word)newnode;

    if (info->type == CAF) {
      oldnode->payload[1] = (Word)G_cap0->static_objs;
      G_cap0->static_objs = oldnode;
    }
    
    T->last_result = (Word)newnode;
    goto do_return;
  }

 op_RET1:
  // opA = result
  //
  // The return address is on the stack. just jump to it.
  T->last_result = base[opA];
 do_return:
  T->top = base - 3;
  pc = (BCIns*)base[-2];
  base = (Word*)base[-3];
  //printf("new base=%p, Node=%p\n", base, (void*)base[-1]);
  { FuncInfoTable *info = getFInfo((Closure*)base[-1]);
    DBG_RETURN(info, pc);
    code = &info->code;
  }
  DISPATCH_NEXT;

 op_MOV_RES:
  // Copy last function call result into a register.
  //
  // opA = target register
  base[opA] = T->last_result;
  DISPATCH_NEXT;

 op_CALLT:
  {
    DECODE_BC;
    // opA = function
    // opC = no of args
    // opB = argument pointer mask
    u4 callargs = opC; // arguments from this call
    u4 pointer_mask = opB; // pointer mask for callargs
    u4 nargs = callargs; // arguments including PAP arguments
    Closure *fnode = (Closure *)base[opA];
    u4 i;
    recordEvent(EV_CALL, callargs);

    LC_ASSERT(fnode != NULL);
    LC_ASSERT(callargs <= BCMAX_CALL_ARGS);

    FuncInfoTable *info;
    PapClosure *pap = NULL;
    switch (getInfo(fnode)->type) {
    case PAP:
      {
	PapClosure *pap = (PapClosure*)fnode;
	fnode = pap->fun;
	LC_ASSERT(getInfo(fnode)->type == FUN);
	info = getFInfo(fnode);
	nargs += pap->nargs;

	DBG_IND(fprintf(stderr, "calling a PAP%d (%s)\n", pap->nargs, info->name));
      }
      break;
    case FUN:
      info = getFInfo(fnode);
      break;
    case CAF:
    case THUNK:
      {
        // Turn current stack frame into an APK frame and build the
        // EVAL and UPDATE stack frames on top.

        // Arguments are already in place, so we can just move `top`
        // to point above the arguments.
        Word *top = base + callargs;
        ThunkInfoTable *info = (ThunkInfoTable*)getInfo(fnode);
        recordEvent(EV_EVAL_THUNK, 0);

        BCIns *ap_return_pc;
        Closure *ap_closure;
        getApContClosure(&ap_closure, &ap_return_pc, callargs, pointer_mask);
        //getAPKClosure(&ap_closure, &ap_return_pc, callargs);
        
        u4 framesize = info->code.framesize;
        DBG_ENTER(info);
        DBG_STACK;

        if (stackOverflow(T, top, STACK_FRAME_SIZEW + UPDATE_FRAME_SIZEW +
                          framesize)) {
          return INTERP_STACK_OVERFLOW;
        }
        
        base[-1] = (Word)ap_closure;
        top[0] = (Word)base;
        top[1] = (Word)ap_return_pc;
        top[2] = (Word)&stg_UPD_closure;
        top[3] = (Word)fnode;
        top[4] = 0;
        top[5] = (Word)&top[3];
        top[6] = (Word)stg_UPD_return_pc;
        top[7] = (Word)fnode;

        base = top + STACK_FRAME_SIZEW + UPDATE_FRAME_SIZEW;
        T->top = base + framesize;
        code = &info->code;
        pc = info->code.code;
        DISPATCH_NEXT;
      }
    default:
      fprintf(stderr, "FATAL: Function argument to CALLT not FUN or PAP.\n");
      exit(1);
    }

    // At this point we have the invariants:
    //   - fnode is a pointer to a FUN
    //   - info is its info table
    //   - nargs is the *total* numbers of arguments applied (not just
    //      from this instruction)

    if (nargs < info->code.arity) { // Partial application
      PapClosure *new_pap = allocClosure(wordsof(PapClosure) + nargs);
      setInfo(new_pap, (InfoTable*)&stg_PAP_info);
      new_pap->arity = info->code.arity - nargs;
      new_pap->nargs = nargs;
      new_pap->fun = fnode;

      DBG_IND(fprintf(stderr, "Creating PAP = %s, nargs = %d, arity = %d\n",
		     info->name, new_pap->nargs, new_pap->arity));


      if (pap != NULL) { // Some arguments come from an existing PAP
	for (i = 0; i < pap->nargs; i++)
	  new_pap->payload[i] = pap->payload[i];
      }

      int papargs = pap != NULL ? pap->nargs : 0;
      // Copy rest (registers r0 ... r{callargs-1})
      for (i = 0; i < callargs; i++)
        new_pap->payload[i + papargs] = base[i];

      // return pointer to new pap
      T->last_result = (Word)new_pap;
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
      Word *top = T->top;

      // Adjust pointer mask to match extra_args.
      pointer_mask >>= callargs - extra_args;

      DBG_IND(fprintf(stderr, " ... overapplication: %d + %d\n",
		     immediate_args, extra_args));

      // 1. Calculate where new frame must start.
      top = base + extra_args + 1;

      u4 framesize = info->code.framesize;
      if (stackOverflow(T, top, STACK_FRAME_SIZEW + framesize)) {
	return INTERP_STACK_OVERFLOW;
      }

      u4 pap_args = pap ? pap->nargs : 0;

      // 2. Rotate immediate and extra arguments:
      //
      // -+----+----+-   -+----+----+----+-   -+
      //  | i0 | i1 | ... | iN | e0 | e1 | ... | eM
      // -+----+----+-   -+----+----+----+-   -+
      //     '------------------------|----------.
      //         .--------------------'          |
      //         v                               v
      // -+----+----+-   -+----+----+------   -+----
      //  |    |    | ... | eM |    | frame ...|
      // -+----+----+-   -+----+----+------   -+----
      //
      if (top + pap_args + STACK_FRAME_SIZEW >= base + callargs) {
	// No overlap.  Copy immediate arguments up
	for (i = 0; i < immediate_args; i++)
	  top[STACK_FRAME_SIZEW + pap_args + i] = base[i];

	// Copy down extra arguments
	for (i = 0; i < extra_args; i++)
	  base[i] = base[i + immediate_args];

      } else if (immediate_args < extra_args) {

        // a. Save immediate args to temporary buffer
        for (i = 0; i < immediate_args; i++)
          callt_temp[i] = base[i];

        // b. Move extra args into place
        for (i = 0; i < extra_args; i++)
          base[i] = base[i + immediate_args];

        // c. Move immediate args into place
        for (i = 0; i < immediate_args; i++)
          top[STACK_FRAME_SIZEW + pap_args + i] = callt_temp[i];

      } else { // immediate_args >= extra_args

        // a. Save extra args to temporary buffer
        for (i = 0; i < extra_args; i++)
          callt_temp[i] = base[i + immediate_args];

        // b. Move immediate args into place
        for (i = 0; i < immediate_args; i++)
          top[STACK_FRAME_SIZEW + pap_args + i] = base[i];

        // c. Move extra args into place
        for (i = 0; i < extra_args; i++)
          base[i] = callt_temp[i];
      }

      // Add args from PAP (if any)
      for (i = 0; i < pap_args; i++)
        top[STACK_FRAME_SIZEW + i] = pap->payload[i];

      // 3. Fill in rest of stack frame.
      BCIns *ap_return_pc;
      Closure *ap_closure;
      getApContClosure(&ap_closure, &ap_return_pc, extra_args, pointer_mask);
      base[-1] = (Word)ap_closure;

      top[0] = (Word)base;
      top[1] = (Word)ap_return_pc;
      top[2] = (Word)fnode;

      DBG_ENTER(info);
      //printFrame(base, top);

      base = top + 3;
      T->top = base + framesize;
      code = &info->code;
      pc = info->code.code;

      DBG_STACK;
      DISPATCH_NEXT;

    } else { // Exact application
      u4 curframesize = T->top - base;
      u4 newframesize = info->code.framesize;

      DBG_ENTER(info);

      if (newframesize > curframesize) {
        if (stackOverflow(T, base, newframesize)) {
          return INTERP_STACK_OVERFLOW;
        } else {
          T->top = base + newframesize;
        }
      }

      if (!pap) {
	// Arguments already in place.
      } else {
        int i;
	// Copy up arguments (stack check already done above)
	for (i = callargs - 1; i >= 0; i --)
	  base[pap->nargs + i] = base[i];
	// Fill in arguments from PAP
	for (i = 0; i < pap->nargs; i++)
	  base[i] = pap->payload[i];
      }

      base[-1] = (Word)fnode;
      code = &info->code;
      pc = info->code.code;

      DBG_STACK;
      DISPATCH_NEXT;
    }
  }

 op_CALL:
  {
    // opA = function
    // opB = argument pointer mask
    // opC = no of argumenst
    // following bytes: argument regs, bitmask
    DECODE_BC;
    // Arguments from this call instruction
    u4       callargs = opC;
    u4       pointer_mask = opB;  // of the arguments
    recordEvent(EV_CALL, callargs);
    // Total number of arguments, including PAP arguments.
    u4       nargs = callargs;
    Closure *fnode = (Closure *)base[opA];
    Word    *top   = T->top;
    u4 i;

    LC_ASSERT(fnode != NULL);
    LC_ASSERT(callargs < BCMAX_CALL_ARGS);

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
    case CAF:
    case THUNK:
      // If the function is a thunk:
      //
      // 1. push application continuation,
      // 2. Put EVAL and UPDATE frames on top.
      {
        ThunkInfoTable *info = (ThunkInfoTable*)getInfo(fnode);
	u4 framesize = info->code.framesize;;
        BCIns *ap_return_pc;
        Closure *ap_closure;
	int i;
	u1 *args = (u1*)pc;

        recordEvent(EV_EVAL_THUNK, 0);
        DBG_ENTER(info);
        DBG_STACK;

        getApContClosure(&ap_closure, &ap_return_pc, callargs, pointer_mask);

	// Build APK frame
	top[0] = (Word)base;
	top[1] = (Word)(pc + BC_ROUND(nargs - 1) + 1);
	top[2] = (Word)ap_closure;
	for (i = 0; i < nargs; i++, args++)
	  top[3 + i] = *args;

	// Put UPDATE and EVAL frames on top
	top[3 + nargs + 0] = (Word)&top[3];
	top[3 + nargs + 1] = (Word)ap_return_pc;
	top[3 + nargs + 2] = (Word)&stg_UPD_closure;
	top[3 + nargs + 3] = (Word)fnode; // reg0
	top[3 + nargs + 4] = 0;           // reg1
	top[3 + nargs + 5] = (Word)&top[3 + nargs + 3];
	top[3 + nargs + 6] = (Word)stg_UPD_return_pc;
	top[3 + nargs + 7] = (Word)fnode;

	base = &top[3 + nargs + 8];
	T->top = base + framesize;
	code = &info->code;
	pc = info->code.code;

	DISPATCH_NEXT;
      }
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

      PapClosure *new_pap = allocClosure(wordsof(PapClosure) + nargs);
      setInfo(new_pap, (InfoTable*)&stg_PAP_info);
      new_pap->arity = info->code.arity - nargs;
      new_pap->nargs = nargs;
      new_pap->fun   = fnode;

      DBG_IND(fprintf(stderr, "Creating PAP = %s, nargs = %d, arity = %d\n",
		     info->name, pap->nargs, new_pap->arity));

      if (pap != NULL) {
        // Copy first few args from old PAP
        for (i = 0; i < pap->nargs; i++)
          new_pap->payload[i] = pap->payload[i];

        // Copy rest
        u1 *args = (u1*)pc;
        LC_ASSERT(LC_ARCH_ENDIAN == LAMBDACHINE_LE);
        for (i = pap->nargs; i < nargs; i++, args++)
          new_pap->payload[i] = base[*args];
      }

      // Return the PAP
      T->last_result = (Word)new_pap;
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
    BCIns *return_pc = pc + BC_ROUND(nargs) + 1;
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
      pointer_mask >>= callargs - extra_args;  // now matches extra_args

      DBG_IND(fprintf(stderr, " ... overapplication: %d + %d\n",
		     immediate_args, extra_args));
      DBG_ENTER(info);

      u4 ap_frame_size = STACK_FRAME_SIZEW + extra_args + 1;
      if (stackOverflow(T, top, STACK_FRAME_SIZEW + framesize + ap_frame_size)) {
	return INTERP_STACK_OVERFLOW;
      }

      top[0] = (Word)base;
      top[1] = (Word)return_pc;
      Closure *ap_closure;
      // Note the modification of `return_pc`.
      getApContClosure(&ap_closure, &return_pc, extra_args, pointer_mask);
      top[2] = (Word)ap_closure;
      saved_base = &top[3];

      u1 *args = (u1*)pc;
      args += immediate_args;
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
	return INTERP_STACK_OVERFLOW;
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
    u1 *arg = (u1*)pc;
    for (i = 0; i < callargs; i++, arg++) {
      top[arg0pos + i] = base[*arg];
    }

    T->base = base = top + STACK_FRAME_SIZEW;
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
    // C = number of arguments (*excluding* function), always >= 1
    // B = pointer mask for arguments
    u4 nargs = opC;
    u4 pointer_mask = opB;
    u4 i;
    recordEvent(EV_ALLOC, nargs + 1);

    LC_ASSERT(nargs >= 1);

    Closure *cl = allocClosure(wordsof(ClosureHeader) + nargs + 1);
    InfoTable *info = getApInfoTable(nargs, pointer_mask);
    setInfo(cl, info);

    u1 *args = (u1 *)pc;
    pc += 1 + BC_ROUND(nargs + 1);
    for (i = 0; i < nargs + 1; i++, args++)
      cl->payload[i] = base[*args];

    base[opA] = (Word)cl;
    DISPATCH_NEXT;
  }

 op_INITF:
  return INTERP_UNIMPLEMENTED;
}

int
stackOverflow(Thread* T, Word* top, u4 increment)
{
  if (T->stack + T->stack_size >= top + increment)
    return 0;
  else
    return 1;
}

void
printStack(FILE *stream, Word *base, Word *bottom)
{
  fprintf(stream, ">>> Stack = ");
  while (base > bottom + 1) {
    FuncInfoTable *i = getFInfo((Closure*)base[-1]);
    fprintf(stream, "%s : ", i->name);
    base = (Word*)base[-3];
  }
  fprintf(stream, "[]\n");
}

void printSlot(FILE *, Word *slot);

void
printFrame(FILE *stream, Word *base, Word *top)
{
  u4 i = -1;
  fprintf(stream, "[%p](%ld)", base, (long)(top - base));
  base--;
  while (base < top) {
    fprintf(stream, " %d:", i);
    printSlot(stream, base);
    ++base; ++i;
  }
  fprintf(stream, "\n");
}

void shortName(char *rslt, u4 maxlen, const char *str);

void
printSlot(FILE *stream, Word *slot)
{
  if (looksLikeClosure((void*)*slot)) {
    Closure *cl = (Closure*)(*slot);
    ConInfoTable *info = (ConInfoTable*)getInfo(cl);
    char name[10];
    shortName(name, 10, info->name);
    if (name[0] == 'I' && name[1] == '#') {
      fprintf(stream, "[I# %" FMT_Word "]", cl->payload[0]);
    } else {
      fprintf(stream, "[%s]", name);
    }
  } else if (looksLikeInfoTable((void*)*slot)) {
    ConInfoTable *info = (ConInfoTable*)*slot;
    char name[10];
    shortName(name, 10, info->name);
    fprintf(stream, "<%s>", name);
  } else {
    fprintf(stream, "$%" FMT_WordX, *slot);
  }
  fflush(stream);
}

void
shortName(char *rslt, u4 maxlen, const char *str)
{
  u4 i, n = 0, last_dot = 0;
  const char *p = str;
  while (*p != 0 && *p != '`') {
    if (*p == '.') last_dot = n + 1;
    p++; n++;
  }
  for (i = 0, p = str + last_dot;
       (i < maxlen - 1) && i < n - last_dot;
       i++, p++) {
    rslt[i] = *p;
  }
  rslt[i] = '\0';
}

void printIndent(FILE *stream, int i, char c)
{
  while (i-- > 0) {
    fputc(c, stream);
  }
}

void
printFullStack(FILE *stream, Word *base, Word *top)
{
  while (base) {
    printFrame(stream, base, top);
    top = (Word*)&base[-3];
    base = (Word*)base[-3];
  }
}

// Invalidate slots that won't be needed after execution returns to
// this place.  Useful for debugging.
//
// If ret_pc != NULL, then the range is assumed to be a stack frame
// and ret_pc is used to extract the associated liveness information.
void
invalidateDeadSlots(Word *from, Word *to, BCIns *ret_pc)
{
  DBG_PR("INVALIDATING: %p..%p", from, to);
  IF_DBG_LVL(0,printInlineBitmap(stderr, ret_pc - 1));
  const u2 *mask = ret_pc ? getLivenessMask(ret_pc) : NULL;
  u2 m = mask ? *mask++ : 0;
  int vbits = 15; // valid mask bits left
  while (from < to) {
    if ((m & 1) == 0) { // not live
      DBG_PR("INV: %p\n", from);
      *from = 0;
    }
    from++;
    m >>= 1;
    vbits--;
    if (vbits == 0) {
      m = mask ? *mask++ : 0;
      vbits = 15;
    }
  }
}
