#include "capability.hh"
#include "bytecode.hh"
#include "thread.hh"
#include "objects.hh"
#include "miscclosures.hh"

#include <iomanip>
#include <string.h>

_START_LAMBDACHINE_NAMESPACE

#define DLOG(...) \
  if (DEBUG_COMPONENTS & DEBUG_INTERPRETER) { \
    cerr.flush(); fprintf(stderr, "IP: " __VA_ARGS__); fflush(stderr); }

#if (DEBUG_COMPONENTS & DEBUG_INTERPRETER) != 0
#define dout cerr
#else
#define dout 0 && cerr
#endif

using namespace std;

static BcIns reload_state_code[1] = { BcIns::ad(BcIns::kSYNC, 0, 0) };

Capability::Capability(MemoryManager *mm)
  : mm_(mm), currentThread_(NULL),
    static_roots_(NULL),
    reload_state_pc_(&reload_state_code[0]),
    counters_(HOT_THRESHOLD), // TODO: initialise from Options
    flags_() {
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
bool isStartOfTrace(BcIns *srcPc, BcIns *dstPc,
                    BranchType branchType) {
  return dstPc < srcPc && (dstPc->opcode() == BcIns::kFUNC ||
                           branchType == kReturn);
}

// It's very important that we inline this because it takes so many
// arguments.
inline BcIns *
Capability::interpBranch(BcIns *srcPc, BcIns *dstPc,
                         Word *&base,
                         BranchType branchType,
                         Thread *&T,
                         char *&heap, char *&heaplim,
                         const AsmFunction *&dispatch,
                         const AsmFunction *&dispatch2,
                         const AsmFunction *dispatch_debug,
                         const Code *&code)
{
#if !LC_JIT
  return dstPc;
#else
  if (LC_UNLIKELY(isRecording())) {
    return dstPc;
  } else {
    if (isStartOfTrace(srcPc, dstPc, branchType)) {
      Fragment *F = jit_.traceAt(dstPc);
      if (F != NULL) {

        // Enter the trace.
        if (DEBUG_COMPONENTS & DEBUG_TRACE_RECORDER) {
          cerr << COL_YELLOW << "TRACE: " << dstPc << COL_RESET << endl;
        }

        T->sync(dstPc, base);
        asmEnter(F->traceId(), T, (Word *)heap, (Word*)heaplim,
                 T->stackLimit(), F->entry());
        heap = (char *)traceExitHp_;
        heaplim = (char *)traceExitHpLim_;

        BcIns *pc = NULL;
        T = currentThread_;
        dispatch = dispatch_; dispatch2 = dispatch_;
        base = T->base(); pc = T->pc();

        if (isEnabledBytecodeTracing() ||
            ((DEBUG_COMPONENTS & DEBUG_TRACE_RECORDER) && isRecording()))
          dispatch = dispatch_debug;

        // Reload code/KBASE
        Closure *cl = (Closure *)base[-1];
        code = ((CodeInfoTable *)cl->info())->code();

        return pc;

      } else if (counters_.tick(dstPc) &&
                 dstPc != MiscClosures::stg_UPD_return_pc) {
        currentThread_->sync(dstPc, base);

        if (DEBUG_COMPONENTS & DEBUG_TRACE_RECORDER) {
          Closure *cl = (Closure *)base[-1];
          cerr << COL_GREEN << "HOT: " << dstPc;
          if (branchType == kReturn) {
            cerr << " return from " << srcPc << " to " << dstPc
                 << " " << cl->info()->name();
          } else {
            cerr << " in " << cl->info()->name();
          }
          cerr << COL_RESET << endl;
        }

        setState(STATE_RECORD);
        jit_.beginRecording(this, dstPc, base, branchType == kReturn);

        // We need to ensure that the interpreter reloads its state.
        // So we return a PC that points to the SYNC instruction.  This
        // reloads all interpreter state from the capability.
        return reload_state_pc_;
      }
    }
    return dstPc;
  }
#endif
}

void Capability::setState(int state) {
  switch (state) {
  case STATE_INTERP:
    dispatch_ = dispatch_normal_;
    flags_.clear(kRecording);
    break;
  case STATE_RECORD:
    dispatch_ = dispatch_record_;
    flags_.set(kRecording);
    break;
  default:
    cerr << "FATAL: setState invalid state: " << state << endl;
    exit(EXIT_FAILURE);
  }
}

void Capability::finishRecording() {
  // TODO: Install recorded trace if successful.
  setState(STATE_INTERP);
}

static inline
bool stackOverflow(Thread *T, Word *top, u4 increment) {
  // The implementation of EVAL currently needs to simulate a return
  // from a function.  Since we store return results inside the frame
  // the returned function (i.e., the frame is now unused), we need to
  // make sure this stack space is valid even if we did not just
  // return from a function.
  u4 headroom = FRAME_SIZE + 1;
  return T->stackLimit() < (top + increment + headroom);
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

extern Word *traceDebugLastHp;

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

  static const AsmFunction dispatch_record[] = {
#   define BCIMPL(name, _) &&record,
    BCDEF(BCIMPL)
#   undef BCIMPL
  };

  if (mode == kModeInit) {
    dispatch_ = dispatch_normal;
    dispatch_normal_ = dispatch_normal;
    dispatch_record_ = dispatch_record;
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

# define DISPATCH_NEXT \
  opcode = pc->opcode(); \
  opA = pc->a(); \
  opC = pc->d(); \
  ++pc; \
  goto *dispatch[opcode]

# define BRANCH_TO(dst_pc, branch_type) \
  pc = interpBranch(pc, (dst_pc), base, (branch_type), \
                    T, heap, heaplim, dispatch, dispatch2, dispatch_debug, code); \
  DISPATCH_NEXT;

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
    cerr << '[' << setfill(' ') << setw(3) << dec
         << depth << ':' << framesize << "] ... ";
    for (u4 i = 0; i < framesize; ++i) {
      cerr << i << ':';
      if (flags_.get(kDecodeClosures) &&
          mm_->inRegions((void *)base[i]) &&
          mm_->looksLikeClosure((void *)base[i]))
        printClosureShort(cerr, (Closure *)base[i]);
      else
        cerr << hex << base[i] << dec;
      cerr << ' ';
    }
    cerr << endl;
    cerr << '[' << setfill(' ') << setw(3) << depth << ':' << framesize << "] ";
    BcIns::debugPrint(cerr, pc, true, NULL, code);
  }
  // Dispatch actual instruction.
  // Note that opA and opC/D have already been decoded and
  // are passed on as the same values that we received.
  opcode = pc->opcode();
  ++pc;
  goto *dispatch2[opcode];

record: {
    // don't change opC
    if (LC_UNLIKELY(jit_.recordIns(pc - 1, base, code))) {
      currentThread_->sync(pc - 1, base);
      finishRecording();
      goto op_SYNC;
    } else {
      opcode = (pc - 1)->opcode();
      goto *dispatch_normal[opcode];
    }
  }

  //
  // ----- Bytecode Implementations ----------------------------------
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

op_ISLTU:
  DECODE_AD;
  ++pc;
  if ((Word)base[opA] < (Word)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

op_ISGEU:
  DECODE_AD;
  ++pc;
  if ((Word)base[opA] >= (Word)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

op_ISLEU:
  DECODE_AD;
  ++pc;
  if ((Word)base[opA] <= (Word)base[opC])
    pc += (pc - 1)->j();
  DISPATCH_NEXT;

op_ISGTU:
  DECODE_AD;
  ++pc;
  if ((Word)base[opA] > (Word)base[opC])
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
    Closure *cl = (Closure *)base[opB];
    base[opA] = cl->payload(opC - 1);
    DISPATCH_NEXT;
  }

op_LOADFV:
  // TODO: This instruction becomes unnecessary if base[0] = Node.
  // A = target
  // C/D = field offset, 1-based index!  TODO: fix this
  {
    DECODE_AD;
    Closure *node = (Closure *)base[-1];
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

op_BNOT:
  DECODE_AD;
  base[opA] = ~base[opC];
  DISPATCH_NEXT;

op_BAND:
  DECODE_BC;
  base[opA] = base[opB] & base[opC];
  DISPATCH_NEXT;

op_BOR:
  DECODE_BC;
  base[opA] = base[opB] | base[opC];
  DISPATCH_NEXT;

op_BXOR:
  DECODE_BC;
  base[opA] = base[opB] ^ base[opC];
  DISPATCH_NEXT;

op_BSHL:
  DECODE_BC;
  base[opA] = base[opB] << (int)base[opC];
  DISPATCH_NEXT;

op_BSHR:
  DECODE_BC;
  base[opA] = base[opB] >> (int)base[opC];
  DISPATCH_NEXT;

op_BSAR:
  DECODE_BC;
  base[opA] = (WordInt)base[opB] >> (int)base[opC];
  DISPATCH_NEXT;

op_BROL: {
    DECODE_BC;
    Word x = base[opB];
    int shift = (int)base[opC];
    base[opA] = (x << shift) | (x >> (sizeof(Word) * 8 - shift));
    DISPATCH_NEXT;
  }

op_BROR: {
    DECODE_BC;
    Word x = base[opB];
    int shift = (int)base[opC];
    base[opA] = (x >> shift) | (x << (sizeof(Word) * 8 - shift));
    DISPATCH_NEXT;
  }

op_PTROFSC:
  DECODE_BC;
  base[opA] = ((char*)base[opB])[(WordInt)base[opC]];
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
    Closure *cl = (Closure *)heap;
    BUMP_HEAP(1);
    ++pc;
    cl->setInfo((InfoTable *)base[opB]);
    cl->setPayload(0, base[opC]);
    base[opA] = (Word)cl;
    DISPATCH_NEXT;
  }

op_ALLOC:
  // A = target
  // B = itbl
  // C = payload size
  // payload regs in little endian order
  {
    DECODE_BC;
    Closure *cl = (Closure *)heap;
    const u1 *arg = (const u1 *)pc;

    BUMP_HEAP(opC);
    cl->setInfo((InfoTable *)base[opB]);
    for (u4 i = 0; i < opC; ++i) {
      // cerr << "payload[" << i << "]=base[" << (int)*arg << "] ("
      //      << (Word)base[*arg] << ")" << endl;
      cl->setPayload(i, base[*arg++]);
    }
    pc += BC_ROUND(opC) + 1 /* bitmap */;

    // This MUST come after payload initialization.
    base[opA] = (Word)cl;
    DISPATCH_NEXT;
  }

op_ALLOCAP:
  // TODO: Any instance of ALLOCAP could be resolved statically.  It
  // could therefore be resolved by the loader.
  {
    DECODE_BC;
    // A = result
    // B = pointer mask for arguments
    // C = number of arguments *excluding* function
    u4 nargs = opC;
    u4 pointerMask = opB;
    const u1 *args = (const u1 *)pc;
    LC_ASSERT(nargs > 0);

    Closure *cl = (Closure *)heap;
    BUMP_HEAP(nargs + 1);
    cl->setInfo(MiscClosures::getApInfo(nargs, pointerMask));
    for (u4 i = 0; i < nargs + 1; ++i, ++args) {
      cl->setPayload(i, base[*args]);
    }

    LC_ASSERT(!isConstructor((Closure *)cl->payload(0)));

    pc += BC_ROUND(nargs + 1) + 1 /* bitmap */;

    base[opA] = (Word)cl;
    DISPATCH_NEXT;
  }

heapOverflow:
  if (isRecording()) jit_.requestAbort();
  --pc;
  // Convention: If GC is needed, T->pc points to the instruction that
  // tried to allocate.
  T->sync(pc, base);
  DLOG("Heap Block Overflow: %p of %p\n", heap, heaplim);
  mm_->bumpAllocatorFull(&heap, &heaplim, this);
  // re-dispatch last instruction
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
      tnode = (Closure *)tnode->payload(0);
    }

    if (tnode->isHNF()) {
      T->top_[FRAME_SIZE] = (Word)tnode;
      ++pc;  // skip live-out info
      DISPATCH_NEXT;
    } else {
      CodeInfoTable *info = static_cast<CodeInfoTable *>(tnode->info());
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

      opC = 0;                  // No arguments.
      BRANCH_TO(code->code, kCall);
    }
  }

op_LOADK: {
    DECODE_AD;
    u2 lit_id = opC;
    LC_ASSERT(lit_id < code->sizelits);
    base[opA] = code->lits[lit_id];
    DISPATCH_NEXT;
  }

op_RET1:
  DECODE_AD;
  base[0] = base[opA];

op_RETN:
  // Arguments are already in place.  We only add some sanity checks
  // here.
  DECODE_AD;
  LC_ASSERT(&base[opA] <= T->top_);

do_return: {
    T->top_ = base - 3;
    BcIns *dst_pc = (BcIns *)base[-2];
    base = (Word *)base[-3];
    {
      Closure *node = (Closure *)base[-1];
      LC_ASSERT(mm_->looksLikeClosure(node));
      CodeInfoTable *info = static_cast<CodeInfoTable *>(node->info());
      DLOG("RETURN %s\n", info->name());
      code = info->code();
      LC_ASSERT(code->code < dst_pc && dst_pc < code->code + code->sizecode);
    }
    opC = dst_pc->d();
    BRANCH_TO(dst_pc, kReturn);
  }

op_IRET:
  base[0] = base[opA];
  goto do_return;

op_UPDATE: {
    Closure *oldnode = (Closure *)base[opA];
    Closure *newnode = (Closure *)base[opC];
    InfoTable *info = oldnode->info();
    LC_ASSERT(oldnode != NULL && mm_->looksLikeClosure(oldnode));
    LC_ASSERT(newnode != NULL && mm_->looksLikeClosure(newnode));

    oldnode->setInfo(MiscClosures::stg_IND_info);
    oldnode->setPayload(0, (Word)newnode);

    if (info->type() == CAF) {
      oldnode->setPayload(1, (Word)static_roots_);
      static_roots_ = oldnode;
    }

    DISPATCH_NEXT;
  }

op_MOV_RES:
  DECODE_AD;
  base[opA] = T->top_[FRAME_SIZE + opC];
  DISPATCH_NEXT;

op_CALL: {
    // opA = function
    // opB = argument pointer mask
    // opC = no of arguments
    // following bytes: argument regs, bitmask
    DECODE_BC;
    u4 callargs = opC;
    u4 pointer_mask = opB;
    u4 nargs;
    Closure *fnode;
    Word *top;

    //op_CALL_retry:
    nargs = callargs;
    fnode = (Closure *)base[opA];
    top = T->top();

    while (fnode->isIndirection()) {
      fnode = (Closure *)fnode->payload(0);
    }

    LC_ASSERT(fnode != NULL);
    LC_ASSERT(mm_->looksLikeClosure(fnode));
    LC_ASSERT(callargs < BcIns::kMaxCallArgs);
    LC_ASSERT(fnode->info()->type() == FUN ||
              fnode->info()->type() == CAF ||
              fnode->info()->type() == THUNK ||
              fnode->info()->type() == PAP);

    CodeInfoTable *info = (CodeInfoTable *)fnode->info();

    DLOG("   ENTER: %s\n", info->name());

    if (stackOverflow(T, top, kStackFrameWords + nargs))
      goto stack_overflow;

    // Each additional argument requires 1 byte, we pad to multiples
    // of an instruction.  The liveness mask follows.
    BcIns *return_pc = pc + BC_ROUND(nargs) + 1;
    Word  *oldbase = base;

    u1 *args = (u1 *)pc;
    pushFrame(&top, &base, return_pc, fnode, nargs);

    for (u4 i = 0; i < callargs; ++i, ++args) {
      base[i] = oldbase[*args];
    }

    T->top_ = top;
    code = info->code();
    opC = (nargs & 0xff) | (pointer_mask << 8);
    goto generic_apply;
  }

op_CALLT: {
    DECODE_BC;
    // opA = function
    // opC = no of args
    // opB = argument pointer mask
    u4 callargs = opC; // arguments from this call
    u4 pointer_mask = opB; // pointer mask for callargs
    u4 nargs = callargs; // arguments including PAP arguments
    //    recordEvent(EV_CALL, callargs);
    Closure *fnode;
    //  op_CALLT_retry:

    fnode = (Closure *)base[opA];

    //  op_CALLT_IND_retry:

    LC_ASSERT(fnode != NULL);
    LC_ASSERT(mm_->looksLikeClosure(fnode));
    LC_ASSERT(callargs < BcIns::kMaxCallArgs);

    while (fnode->isIndirection()) {
      fnode = (Closure *)fnode->payload(0);
    }

    LC_ASSERT(fnode->info()->type() == FUN ||
              fnode->info()->type() == CAF ||
              fnode->info()->type() == THUNK ||
              fnode->info()->type() == PAP);

    // TODO: Remove indirections or make them callable, same for
    // blackhole.

    CodeInfoTable *info = (CodeInfoTable *)fnode->info();

    DLOG("   ENTER: %s\n", info->name());

    if (stackOverflow(T, base, nargs)) {
      goto stack_overflow;
    }

    // Arguments are already in place.  Just dispatch to target.

    base[-1] = (Word)fnode;
    code = info->code();
    opC = (nargs & 0xff) | (pointer_mask << 8);
    goto generic_apply;
  }

op_IFUNC:
  // IFUNC is functionally equivalent to FUNC. It is treated differently
  // by the trace selector though: an IFUNC is never considered as a
  // possible trace root.
op_FUNC:
op_FUNCPAP:
  LC_ASSERT(opA == T->top_ - base);
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
    u2 *table = (u2 *)pc;
    pc += (num_cases + 1) >> 1;

    LC_ASSERT(mm_->looksLikeClosure(cl));
    LC_ASSERT(cl->info()->type() == CONSTR);

    u2 tag = cl->tag() - 1;  // tags start at 1

    LC_ASSERT(tag < num_cases);

    u2 offs = table[tag];
    pc += offs;

    DISPATCH_NEXT;
  }

op_SYNC:
  // Synchronises the interpreter state from the capability.
  // Does NOT update the memory manager state!
  LOAD_STATE_FROM_CAP;
  if (isEnabledBytecodeTracing() ||
      ((DEBUG_COMPONENTS & DEBUG_TRACE_RECORDER) && isRecording()))
    dispatch = dispatch_debug;
  // TODO: this is hacky.
  DISPATCH_NEXT;

op_JFUNC: {
    Fragment *F = jit_.lookupFragment(pc - 1);
    T->sync(pc - 1, base);
    //    traceDebugLastHp = (Word *)heap;
    asmEnter(F->traceId(), T,
             (Word *)heap, (Word *)heaplim,
             T->stackLimit(), F->entry());
    heap = (char *)traceExitHp_;
    heaplim = (char *)traceExitHpLim_;

    LOAD_STATE_FROM_CAP;
    if (isEnabledBytecodeTracing() ||
        ((DEBUG_COMPONENTS & DEBUG_TRACE_RECORDER) && isRecording()))
      dispatch = dispatch_debug;

    // Reload code/KBASE
    Closure *cl = (Closure *)base[-1];
    code = ((CodeInfoTable *)cl->info())->code();

    DISPATCH_NEXT;
  }

op_LOADBH:
op_INITF:
op_KINT:
op_NEW_INT:
op_CASE_S:
op_JRET:
  cerr << "\nERROR: Unimplemented instruction: " << (pc - 1)->name() << endl;
  // not_yet_implemented:
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
  cerr << "\nERROR: Stack overflow.\n";
  return kInterpStackOverflow;

generic_apply: {
    Closure *fnode = (Closure *)base[-1];
    u4 given_args = opC & 0xff;
    u4 pointer_mask = opC >> 8;

    // INV: code == fnode->code()

    // Stack at this point:
    //
    //     -+-------+-----+-  -+-----+
    //      | fnode | a_1 | .. | a_N |  where N = given_args
    //     -+-------+-----+-  -+-----+
    //
    // Also, bit(i-1) of ptr_mask describes pointerhood of a_i.

  generic_apply_retry:
    LC_ASSERT((Word)fnode == base[-1]);
    LC_ASSERT(code == ((CodeInfoTable *)fnode->info())->code());

    switch (fnode->info()->type()) {
    case PAP: {
      //
      //                +-------+-----+-   -+-----+
      //     fnode *--> | pap_f | p_1 | ... | p_M | where M = pap->nargs_;
      //                +-------+-----+-   -+-----+
      //
      // We need to transform the stack into:
      //
      //     -+-------+-----+-  -+-----+-----+-  -+-----+
      //      | pap_f | p_1 | .. | p_M | a_1 | .. | a_N |  where N = given_args
      //     -+-------+-----+-  -+-----+-----+-  -+-----+
      //
      // And then retry.
      PapClosure *pap = (PapClosure *)fnode;
      LC_ASSERT(pap->info()->type() == PAP);
      u4 papArgs = pap->info_.nargs_;

      dout << "FUNCPAP (args=" << given_args
           << ", ptrs=" << hex << pointer_mask << dec
           << ") PAP=(args=" << papArgs << ")"
           << endl;

      u4 framesize = papArgs + given_args;
      if (stackOverflow(T, base, framesize)) {
        goto stack_overflow;
      }
      T->top_ = base + framesize;

      // Move up a_1 .. a_N
      for (int i = given_args - 1; i >= 0; --i)
        base[papArgs + i] = base[i];

      // Copy p_1 .. p_M onto the stack.
      for (u4 i = 0; i < papArgs; ++i)
        base[i] = pap->payload(i);

      fnode = pap->fun_;
      base[-1] = (Word)fnode;
      pointer_mask <<= papArgs;
      pointer_mask |= pap->info_.pointerMask_;
      given_args += papArgs;

      const CodeInfoTable *info = (CodeInfoTable *)fnode->info();
      code = info->code();
      goto generic_apply_retry;
    }
    case THUNK:
    case CAF: {
      //  1. Turn all current arguments into APK.
      BcIns *apk_return_addr = NULL;
      Closure *apk_closure = NULL;
      MiscClosures::getApCont(&apk_closure, &apk_return_addr,
                              given_args, pointer_mask);
      uint32_t apk_framesize = MiscClosures::apContFrameSize(given_args);
      base[-1] = (Word)apk_closure;
      Word *top = &base[apk_framesize];
      const CodeInfoTable *info = (CodeInfoTable *)fnode->info();
      code = info->code();

      if (stackOverflow(T, base, apk_framesize + kStackFrameWords +
                        kUpdateFrameWords + kStackFrameWords +
                        code->framesize))
        goto stack_overflow;

      pushFrame(&top, &base, apk_return_addr,
                MiscClosures::stg_UPD_closure_addr, 2);
      // 2. Setup update frame.
      base[0] = (Word)fnode;
      base[1] = 0xbadbadff;
      pushFrame(&top, &base, MiscClosures::stg_UPD_return_pc, fnode,
                code->framesize);

      LC_ASSERT(top == base + code->framesize);
      T->top_ = top;
      BRANCH_TO(code->code, kCall);
    }
    case FUN: {
      const CodeInfoTable *info = (CodeInfoTable *)fnode->info();
      uint32_t arity = info->code()->arity;
      code = info->code();

      if (LC_UNLIKELY(arity < given_args)) {

        DLOG("Overapplication (args=%d, arity=%d)\n", given_args, arity);
        // Assume: given_args = M, arity = N
        //
        // Then we want to transform the stack
        //
        //     -+----+----+-  -+----+----+-  -+----+
        //      | f  | a1 | .. | aN |aN+1| .. | aM |
        //     -+----+----+-  -+----+----+-  -+----+
        //
        // into the stack
        //
        //     -+----+----+-  -+----+-------+----+----+-  -+----+
        //      | APK|aN+1| .. | aM | frame |  f | a1 | .. | aN |
        //     -+----+----+-  -+----+-------+----+----+-  -+----+
        //
        u4 extra_args = given_args - arity;
        u4 apk_frame_size = MiscClosures::apContFrameSize(extra_args) + 3;
        if (stackOverflow(T, base, apk_frame_size + given_args))
          goto stack_overflow;

        // We could do some clever swapping scheme here, but it
        // would be very branchy and probably not worth it.

        BcIns *apk_return_addr = NULL;
        Closure *apk_closure = NULL;
        MiscClosures::getApCont(&apk_closure, &apk_return_addr,
                                extra_args, pointer_mask >> arity);

        memmove(&base[apk_frame_size], &base[0],
                given_args * sizeof(Word));
        base[apk_frame_size - 1] = base[-1];
        base[apk_frame_size - 2] = (Word)apk_return_addr;
        base[apk_frame_size - 3] = (Word)&base[0];
        base[-1] = (Word)apk_closure;
        memmove(&base[0], &base[apk_frame_size + arity],
                extra_args * sizeof(Word));
        base = &base[apk_frame_size];

      } else if (LC_UNLIKELY(given_args < arity)) {

        DLOG("Partial application\n");

        u4 pap_size = 1 + given_args;
        PapClosure *pap = (PapClosure *)heap;
        heap += (wordsof(PapClosure) + pap_size) * sizeof(Word);
        while (LC_UNLIKELY(heap > heaplim)) {
          heap -= (wordsof(PapClosure) + pap_size) * sizeof(Word);
          if (isRecording()) jit_.requestAbort();
          // PC points after the CALL/CALLT/EVAL. We're setting the
          // top of stack pointer mask, though, so the GC really only
          // needs the correct base pointer.
          T->sync(pc, base);
          mm_->setTopOfStackMask(pointer_mask);
          mm_->bumpAllocatorFull(&heap, &heaplim, this);
          mm_->setTopOfStackMask(MemoryManager::kNoMask);  // Reset mask.

          // Try again.
          pap = (PapClosure *)heap;
          heap += (wordsof(PapClosure) + pap_size) * sizeof(Word);
        }

        pap->init(MiscClosures::stg_PAP_info, pointer_mask, given_args,
                  fnode);
        for (u4 i = 0; i < given_args; ++i) {
          pap->setPayload(i, base[i]);
        }

        base[0] = (Word)pap;
        goto do_return;
      }

      T->top_ = base + code->framesize;
      BRANCH_TO(code->code, kCall);
    }
    default:
      cerr << "Unsupported closure type for CALL/CALLT:"
           << (int)fnode->info()->type();
      return kInterpUnimplemented;
    }
  }
}

_END_LAMBDACHINE_NAMESPACE
