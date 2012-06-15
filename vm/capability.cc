#include "capability.hh"
#include "bytecode.hh"
#include "thread.hh"
#include "objects.hh"
#include "miscclosures.hh"

#include <iomanip>

_START_LAMBDACHINE_NAMESPACE

#define DLOG(...) \
  if (DEBUG_COMPONENTS & DEBUG_INTERPRETER) { \
    fprintf(stderr, "LD: " __VA_ARGS__); }

using namespace std;

Capability::Capability(MemoryManager *mm)
  : mm_(mm), currentThread_(NULL), flags_(0) {
  interpMsg(kModeInit);
}

Capability::~Capability() {
}

bool Capability::run(Thread *T) {
  LC_ASSERT(T != NULL);
  currentThread_ = T;
  return interpMsg(kModeRun) == kInterpOk;
}

bool Capability::eval(Thread *T, Closure *cl) {
  LC_ASSERT(T != NULL);
  T->setSlot(0, (Word)cl);
  return run(T);
}

static inline
bool stackOverflow(Thread* T, Word* top, u4 increment) {
  return T->stackLimit() < (top + increment);
}

// NOTE: Does not check for stack overflow.
static inline
void pushFrame(Word **top, Word **base, BcIns *ret, Closure *clos,
               u4 framesize) {
  Word *t = *top;
  t[0] = (Word)(*base);
  t[1] = (Word)ret;
  t[2] = (Word)clos;
  *base = &t[3];
  *top = *base + framesize;
}

static const int kStackFrameWords = 3;
static const int kUpdateFrameWords = 5;

Capability::InterpExitCode Capability::interpMsg(InterpMode mode) {
  static const AsmFunction dispatch_normal[] = {
#   define BCIMPL(name, _) &&op_##name,
    BCDEF(BCIMPL)
#   undef BCIMPL
  };

  static const AsmFunction dispatch_debug[] = {
#   define BCIMPL(name, _) &&debug,
    BCDEF(BCIMPL)
#   undef BCIMPL
  };

  if (mode == kModeInit) {
    dispatch_ = dispatch_normal;
    dispatch_normal_ = dispatch_normal;
    return kInterpOk;
  }

  Thread *T;
  const AsmFunction *dispatch, *dispatch2;
  Word *base;
  BcIns *pc;
  u4 opA, opB, opC, opcode;
  char *heap;
  char *heaplim;
  mm_->getBumpAllocatorBounds(&heap, &heaplim);
  // Technically, we only need a pointer to the literals.  But having
  // a pointer to the whole code segment can be useful for debugging.
  const Code *code = NULL;

# define LOAD_STATE_FROM_CAP \
  do { T = currentThread_; \
       dispatch = dispatch_; dispatch2 = dispatch_; \
       base = T->base(); pc = T->pc(); } while (0)

  LOAD_STATE_FROM_CAP;

  if (isEnabledBytecodeTracing())
    dispatch = dispatch_debug;

# define DISPATCH_NEXT_WITH(disp) \
  opcode = pc->opcode(); \
  opA = pc->a(); \
  opC = pc->d(); \
  ++pc; \
  goto *(disp)[opcode]

# define DISPATCH_NEXT DISPATCH_NEXT_WITH(dispatch)

# define DECODE_BC \
  opB = opC >> 8; \
  opC = opC & 0xff;

# define DECODE_AD \
  do { } while(0)


  // Dispatch first instruction.
  DISPATCH_NEXT;
  
  //
  // ----- Special Mode Implementations ------------------------------
  //

 debug:
  --pc;
  {
    size_t depth = base - T->stackStart();
    size_t framesize = T->top() - base;
    cerr << '[' << setfill(' ') << setw(3) << depth << ':' << framesize << "] ";
    BcIns::debugPrint(cerr, pc, true, NULL, NULL);
  }
  DISPATCH_NEXT_WITH(dispatch2);

  //
  // ----- Bytecode Implentations ------------------------------------
  //

 op_ISLT:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] < (WordInt)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

 op_ISGE:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] >= (WordInt)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

 op_ISLE:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] <= (WordInt)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

 op_ISGT:
  DECODE_AD;
  ++pc;
  if ((WordInt)base[opA] > (WordInt)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

 op_ISEQ:
  DECODE_AD;
  ++pc;
  if (base[opA] == base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

 op_ISNE:
  DECODE_AD;
  ++pc;
  if (base[opA] != base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

 op_NOT:
  DECODE_AD;
  base[opA] = ~base[opC];
  DISPATCH_NEXT;

 op_NEG:
  DECODE_AD;
  base[opA] = -(WordInt)base[opC];
  DISPATCH_NEXT;

 op_MOV:
  DECODE_AD;
  base[opA] = base[opC];
  DISPATCH_NEXT;

 op_LOADSLF:
  // TODO: This instruction becomes unnecessary if base[0] = Node
  base[opA] = base[-1];
  DISPATCH_NEXT;

 op_LOADF:
  // A = target
  // B = closure ptr.
  // C = field offset, 1-based indexed!  TODO: fix this
  {
    DECODE_BC;
    Closure *cl = (Closure*)base[opB];
    base[opA] = cl->payload(opC - 1);
    DISPATCH_NEXT;
  }

 op_LOADFV:
  // TODO: This instruction becomes unnecessary if base[0] = Node.
  // A = target
  // C/D = field offset, 1-based index!  TODO: fix this
  {
    DECODE_AD;
    Closure *node = (Closure*)base[-1];
    base[opA] = node->payload(opC - 1);
    DISPATCH_NEXT;
  }

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
  // Signed and unsigned multiplication are actually identical (except
  // for CPU flags).
  base[opA] = (WordInt)base[opB] * (WordInt)base[opC];
  DISPATCH_NEXT;

 op_DIVRR:
  DECODE_BC;
  base[opA] = (WordInt)base[opB] / (WordInt)base[opC];
  DISPATCH_NEXT;

 op_REMRR:
  DECODE_BC;
  base[opA] = (WordInt)base[opB] % (WordInt)base[opC];
  DISPATCH_NEXT;

# define BUMP_HEAP(payloadWords) \
  heap += (1 + (payloadWords)) * sizeof(Word); \
  if (LC_UNLIKELY(heap > heaplim)) { \
    heap -= (1 + (payloadWords)) * sizeof(Word); \
    goto heapOverflow; \
  }

 op_ALLOC1:
  // A = target
  // B = itbl
  // C = payload[0]
  {
    DECODE_BC;
    Closure *cl = (Closure*)heap;
    BUMP_HEAP(1);
    cl->setInfo((InfoTable*)base[opB]);
    cl->setPayload(0, base[opC]);
    base[opA] = (Word)cl;
    ++pc; // skip bitmask
    DISPATCH_NEXT;
  }

 op_ALLOC:
  // A = target
  // B = itbl
  // C = payload size
  // payload regs in little endian order
  {
    DECODE_BC;
    Closure *cl = (Closure*)heap;
    BUMP_HEAP(opC);
    cl->setInfo((InfoTable*)base[opB]);
    const u1 *arg = (const u1 *)pc;
    for (u4 i = 0; i < opC; ++i) {
      // cerr << "payload[" << i << "]=base[" << (int)*arg << "] ("
      //      << (Word)base[*arg] << ")" << endl;
      cl->setPayload(i, base[*arg++]);
    }
    // This MUST come after payload initialization.
    base[opA] = (Word)cl;
    pc += BC_ROUND(opC) + 1 /* bitmap */;
    DISPATCH_NEXT;
  }

 heapOverflow:
  DLOG("Heap Block Overflow: %p of %p\n", heap, heaplim);
  mm_->bumpAllocatorFull(&heap, &heaplim);
  --pc;  // re-dispatch last instruction
  DISPATCH_NEXT;

 op_JMP:
  // Offsets are relative to the current PC which points to the
  // following instruction.  Hence, "JMP 0" is a no-op, "JMP -1" is an
  // infinite loop.
  pc += opC - BcIns::kBranchBias;
  DISPATCH_NEXT;

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
    Closure *tnode = (Closure *)base[opA];
    
    LC_ASSERT(tnode != NULL);
    LC_ASSERT(mm_->looksLikeClosure(tnode));
    
    while (tnode->isIndirection()) {
      tnode = (Closure*)tnode->payload(0);
    }

    if (tnode->isHNF()) {
      T->setLastResult((Word)tnode);
      ++pc;  // skip live-out info
      DISPATCH_NEXT;
    } else {
      CodeInfoTable *info = static_cast<CodeInfoTable*>(tnode->info());
      u4 framesize = info->code()->framesize;
      Word *top = T->top();

      if (stackOverflow(T, top, kStackFrameWords + kUpdateFrameWords
                        + framesize))
        goto stack_overflow;

      Word *top_orig = top;

      BcIns *returnPc = pc + 1;  // skip live-out info
      pushFrame(&top, &base, returnPc, MiscClosures::stg_UPD_closure_addr, 2);
      base[0] = (Word)tnode;
      base[1] = 0xbadbadff;
      pushFrame(&top, &base, MiscClosures::stg_UPD_return_pc, tnode, framesize);

      LC_ASSERT(base == top_orig + kStackFrameWords + kUpdateFrameWords);
      LC_ASSERT(top == base + framesize);
      T->top_ = top;
      code = info->code();

      pc = code->code;
      DISPATCH_NEXT;
    }
  }

 op_FUNC:
  // ATM, this is just a NOP.  It may be overwritten by a JFUNC.
  DISPATCH_NEXT;

 op_LOADK:
  {
    DECODE_AD;
    u2 lit_id = opC;
    LC_ASSERT(lit_id < code->sizelits);
    base[opA] = code->lits[lit_id];
    DISPATCH_NEXT;
  }

 op_RET1:
  DECODE_AD;
  T->setLastResult(base[opA]);

 do_return:  
  T->top_ = base - 3;
  pc = (BcIns*)base[-2];
  base = (Word*)base[-3];
  {
    Closure *node = (Closure*)base[-1];
    LC_ASSERT(mm_->looksLikeClosure(node));
    CodeInfoTable *info = static_cast<CodeInfoTable*>(node->info());
    code = info->code();
    LC_ASSERT(code->code < pc && pc < code->code + code->sizecode);
  }
  DISPATCH_NEXT;

 op_IRET:
  T->setLastResult(base[opA]);
  goto do_return;

 op_UPDATE:
  {
    Closure *oldnode = (Closure*)base[opA];
    Closure *newnode = (Closure*)base[opC];
    LC_ASSERT(oldnode != NULL && mm_->looksLikeClosure(oldnode));
    LC_ASSERT(newnode != NULL && mm_->looksLikeClosure(newnode));
    oldnode->setInfo(MiscClosures::stg_IND_info);
    oldnode->setPayload(0, (Word)newnode);
    
    DISPATCH_NEXT;
  }

 op_MOV_RES:
  DECODE_AD;
  base[opA] = T->lastResult();
  DISPATCH_NEXT;

 op_LOADBH:
 op_INITF:
 op_KINT:
 op_NEW_INT:
 op_ALLOCAP:
 op_CALL:
 op_CALLT:
 op_CASE:
 op_CASE_S:
 op_IFUNC:
 op_JFUNC:
 op_JRET:
 op_SYNC:
  cerr << "Unimplemented instruction: " << (pc-1)->name() << endl;
  T->sync(pc, base);
  mm_->sync(heap, heaplim);
  return kInterpUnimplemented;

 op_STOP:
  T->sync(pc, base);
  mm_->sync(heap, heaplim);
  return kInterpOk;

 stack_overflow:
  T->sync(pc, base);
  mm_->sync(heap, heaplim);
  return kInterpStackOverflow;
}

_END_LAMBDACHINE_NAMESPACE
