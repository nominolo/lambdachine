Cached copy of: http://www.reddit.com/r/programming/comments/badl2/luajit_2_beta_3_is_out_support_both_x32_x64/c0lrus0

LuaJIT's interpreter is fast, because:

  * It's written in assembler.
  * It keeps all important state in registers. No C compiler manages to do that on x86.
  * It uses indirect threading (aka labeled goto in C).
  * It has a very small I-cache footprint (the core of the interpreter fits in 6K).
  * The parser generates a register-based bytecode.
  * The bytecode is really a word-code (32 bit/ins) and designed for fast decoding.
  * Bytecode decode and dispatch is heavily optimized for superscalar CPUs.
  * The bytecode is type-specialized and patched on-the-fly.
  * The dispatch table is patched to allow for debug hooks and trace recording. No need to check for these cases in the fast paths.
  * It uses NaN tagging for object references. This allows unboxed FP numbers with a minimal cache footprint for stacks/arrays. FP stores are auto-tagging.
  * It inlines all fast paths.
  * It uses special calling conventions for built-ins (fast functions).
  * Tons more tuning in the VM ... and the JIT compiler has it's own bag of tricks.

E.g. x=x+1 is turned into the ADDVN instruction. This means it's
specialized for the 2nd operand to be a constant. Here's the x86 code
(+ SSE2 enabled) for this instruction:

    // Prologue for type ABC instructions (others have a zero prologue).
    movzx  ebp, ah                  Decode RC (split of RD)
    movzx  eax, al                  Decode RB (split of RD)

    // The instruction itself.
    cmp    [edx+ebp*8+0x4], -13     Type check of [RB]
    ja     ->lj_vmeta_arith_vn
    movsd  xmm0, [edx+ebp*8]        Load of [RB]
    addsd  xmm0, [edi+eax*8]        Add to [RC]
    movsd  [edx+ecx*8], xmm0        Store in [RA]

    // Standard epilogue: decode + dispatch the next instruction.
    mov    eax, [esi]               Load next bytecode
    movzx  ecx, ah                  Decode RA
    movzx  ebp, al                  Decode opcode
    add    esi, 0x4                 Increment PC
    shr    eax, 0x10                Decode RD
    jmp    [ebx+ebp*4]              Dispatch to next instruction

Yes, that's all of it. I don't think you can do this with less
instructions. This code reaches up to 2.5 ipc on a Core2 and takes 5-6
cycles (2 nanoseconds on a 3 GHz machine).



Cached version of: http://www.reddit.com/r/programming/comments/hkzg8/author_of_luajit_explains_why_compilers_cant_beat/c1w8xyz

BTW: For the LuaJIT/ARM interpreter I had to add even more crazy stuff
to make it fast. The assembler code for the LuaJIT/x86 interpreter is
rather straightforward in comparison. I don't think you're going to
see any compiler generate code like this, anytime soon (not even my
own).

Here's a dump of the ARM dual-number/soft-float machine code for the
ADDVN bytecode of LuaJIT (add of variable + number constant). It gives
a good example of the kind of optimizations that are only possible
with assembler:

    and   r12, r4, lr, lsr #21     // Decode RB * 8
    and   r11, r4, lr, lsr #13     // Decode RC * 8
    ldrd  r0, [r9, r12]            // Load TValue from BASE[RB]
    ldrd  r2, [r5, r11]            // Load TValue from KBASE[RC]
    |ldrb r12, [r6]                // Load next opcode
    cmn   r1, #14                  // 1st operand is integer?
    cmneq r3, #14                  // And 2nd operand is integer?
    bne   >2                       // No, try FP variant
    adds  r0, r0, r2               // Yes, do integer add
    bvs   ->lj_vmeta_arith_vn      // Fallback on overflow

    1:
    |ldr  lr, [r6], #4             // Load next instruction, increment PC
    strd  r0, [r9, r10]            // Store TValue result in BASE[RA]
    |ldr  r12, [r7, r12, lsl #2]   // Load code address for next opcode
    |and  r10, r4, lr, lsr #5      // Pre-decode next RA * 8
    |lsr  r11, lr, #16             // Pre-decode next RD
    |bx   r12                      // Jump to code for next opcode

    2:  // FP variant
    cmn   r1, #14                  // 1st operand is number?
    cmnlo r3, #14                  // And 2nd operand is number?
    bhs   ->lj_vmeta_arith_vn      // Fallback if not
    bl    extern __aeabi_dadd      // Soft-float add
    |ldrb r12, [r6]                // Reload volatile opcode reg
    b <1

  * r4 is pre-initialized to 0x7f8 (255*8), which allows fast decoding
    and scaling of the 8 bit operands inside the 32 bit instruction
    word. The pre-scaling of operands is required for the subsequent
    'ldrd' instruction, which only allows base+offset or base+index
    addressing.

  * 'ldrd' loads a 64 bit value into two consecutive registers. This
    conveniently allows loading a TValue from the stack or the
    constant table with a single instruction. The hi-word has the type
    code, which overlaps with the hi-word of doubles. Similarly,
    'strd' allows storing a TValue in one go -- that's either a double
    or an integer + type code.

  * The type codes are small negative numbers (NaN-tagged values),
    which allows for a fast type check with 'cmn' (compare
    negated). Integers are at -14, other types are at -1..-13, numbers
    occupy the remaining space (hiword of a double).

  * The checks can be chained with predicated instructions, e.g. cmn +
    cmneq + bne (both are integers) or cmn + cmnlo + bhs (both are
    numbers). The fast paths are always the straight line fall-through
    paths, e.g. the integer add in this example.

  * Some other operations, e.g. bit.* allow even more streamlined type
    checks, e.g. cmn + blne to a subroutine that handles the
    (uncommon) non-integer cases. It starts with a bhi to the fallback
    code (not a number) and continues with an inlined conversion from
    numbers to integers.

  * If you carefully look at the load latencies (2 cy) and the early
    register constraints (for addresses and stored values), you'll see
    the above code doesn't have any stalls. All operations are
    carefully interleaved, based on the data dependencies. Even the
    next opcode dispatch (marked with '|') is interleaved with the
    current opcode execution.

  * Also note that the pre-decoding of the operands for the next
    instruction is done in the delay slot of the load of the machine
    code address for the next opcode. The decoded RD is used for many
    instructions, but not for the ADDVN instruction shown here (OTOH
    not doing it would just waste a delay slot).

  * Yes, this bytecode instruction could be split into two
    instructions. One for the integer and FP variant, each. And with
    dynamic bytecode patching to adapt to runtime behavior. But one
    needs a state machine and a dual-variant to prevent infinite
    re-patching due to type instability. That would add too much
    complexity and increase the I-cache footprint a lot, for little
    gain (and ARM has very small caches).

  * The JIT compiler specializes to the runtime type, anyway. And it's
    able to turn that into an adds + bvs for the integer case. The
    overflow check can be eliminated in some cases, which leaves only
    an add instruction. It's a tad more complex in practice, than it
    sounds, though. :-)

---

Q: Why are you decoding RB and RC from the full instruction (lr)
instead of from RD/r11? Does this reduce some data dependency delay?

A: Yes, mainly to avoid the delay, but also for consistency. In some
cases RC is needed earlier than RB, which would overwrite the input
for RB.

Though this data dependency in particular is irrelevant on the simpler
ARM chips, because the indirect branch is not predicted and costs a
fixed number of cycles > 2.

Note that the data dependency for the opcode (r12) is relevant. It's
used as an index in a load (early register), so 2+1 cycles need to be
filled with other stuff. That's why it does an early byte-wide load
for the opcode instead of decoding from the word in lr later on.
