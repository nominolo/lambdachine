Memo 4: GMP Support
===================

## How GHC does it

In GHC most primitives are implemented in C--. Example:

    plusInteger :: Int# -> ByteArray# -> Int# -> ByteArray# -> MPZ#
    type MPZ# = (# Int#, ByteArray#, Word# #)

The `MPZ#` type avoids some heap allocation and is interpreted in the
following way:

    (#  0#, _, 0## #) -> value = 0
    (#  1#, _, w   #) -> value = w
    (# -1#, _, w   #) -> value = -w
    (#  s , d, 0## #) -> value = J# s d

The primitive function then is implemented as

    GMP_TAKE2_RET1(integer_cmm_plusIntegerzh, __gmpz_add)

which is a macro that expands to (annotated):

    integer_cmm_plusIntegerzh (W_ ws1, P_ d1, W_ ws2, P_ d2)
    {
      W_ mp_tmp1;
      W_ mp_tmp2;
      W_ mp_result1;

    again:
      // Manually reserve space for temporaries on the stack. If that
      // fails the garbage collector may have to be invoked.
      STK_CHK_GEN_N (2*SIZEOF_MP_INT + SIZEOF_MP_INT_1LIMB);
      MAYBE_GC(again);

      mp_tmp1    = Sp - 1*SIZEOF_MP_INT;
      mp_tmp2    = Sp - 2*SIZEOF_MP_INT;
      mp_result1 = Sp - 2*SIZEOF_MP_INT - SIZEOF_MP_INT_1LIMB;

      MP_INT_SET_FROM_BA(mp_tmp1,ws1,d1);
      MP_INT_SET_FROM_BA(mp_tmp2,ws2,d2);

      MP_INT_1LIMB_INIT0(mp_result1);

      /* Perform the operation */
      ccall __gmpz_add(mp_result1 "ptr",mp_tmp1  "ptr",mp_tmp2  "ptr");

      MP_INT_1LIMB_RETURN(mp_result1);
    }
    
    /* set mpz_t from Int#/ByteArray# */
    #define MP_INT_SET_FROM_BA(mp_ptr,i,ba)                  \
      MP_INT__mp_alloc(mp_ptr) = W_TO_INT(BYTE_ARR_WDS(ba)); \
      MP_INT__mp_size(mp_ptr)  = W_TO_INT(i);                \
      MP_INT__mp_d(mp_ptr)     = BYTE_ARR_CTS(ba)
      
    /* Initialize 0-valued single-limb mpz_t at mp_ptr */
    #define MP_INT_1LIMB_INIT0(mp_ptr)                       \
      MP_INT__mp_alloc(mp_ptr) = W_TO_INT(1);                \
      MP_INT__mp_size(mp_ptr)  = W_TO_INT(0);                \
      MP_INT__mp_d(mp_ptr)     = (mp_ptr+SIZEOF_MP_INT)

    /* return mpz_t as (# s::Int#, d::ByteArray#, l1::Word# #) tuple
     *
     * semantics:
     *
     *  (#  0, _, 0 #) -> value = 0
     *  (#  1, _, w #) -> value =  w
     *  (# -1, _, w #) -> value = -w
     *  (#  s, d, 0 #) -> value =  J# s d
     *
     */
    #define MP_INT_1LIMB_RETURN(mp_ptr)                    \
      CInt __mp_s;                                         \
      __mp_s = MP_INT__mp_size(mp_ptr);                    \
                                                           \
      if (__mp_s == W_TO_INT(0))                           \
      {                                                    \
        return (0,DUMMY_BYTE_ARR,0);                       \
      }                                                    \
                                                           \
      if (__mp_s == W_TO_INT(-1) || __mp_s == W_TO_INT(1)) \
      {                                                    \
        return (TO_W_(__mp_s),DUMMY_BYTE_ARR,W_[MP_INT__mp_d(mp_ptr)]); \
      }                                                    \
                                                           \
      return (TO_W_(__mp_s),MP_INT_TO_BA(mp_ptr),0)

    #define MP_INT_TO_BA(mp_ptr) \
      (MP_INT__mp_d(mp_ptr)-SIZEOF_StgArrWords)

This creates three `mpz_t` structures on the Haskell stack,
initializes them from a byte array and then calls `__gmpz_add`. This
stores the result in the third temporary which is then examined to
return three results with the semantics of `MPZ#`.

Note that we configured the GMP to use our custom allocator. When GMP
requests the allocation of `N` bytes, this allocator will actually
allocate a byte array on the Haskell heap and return a pointer to the
start of the payload bytes. The `MP_INT_TO_BA` macro uses this to
compute the address of the byte array by simply subtracting the meta
data size from the pointer returned by the GMP function.

On my x64 system `SIZEOF_MP_INT` is 16 bytes.


## Option 1: Stack allocations

As with GHC, we modify the GMP allocator to allocate byte arrays on
the Haskell heap. That part is straightforward. The feature currently
missing are stack-allocated structures. We have two options:

 1. Store them in regular virtual registers. A portable bytecode
    format must specify the number of bytes and then the loader must
    compute the number of registers slots must be used on the target
    architecture.  This requires an instruction `ADDRS dst, reg` to
    get a pointer to the physical memory of a register so that we can
    pass pointers to these structures to C functions.

 2. Store them in a special region that has the same lifetime as the
    stack frame, e.g., on a second stack, or on the C stack. This
    requires an instruction `ALLOCS dst, bytes` to allocate a new stack
    region of the given number of bytes. Store its address in virtual
    register `dst`.
   
In any case we need new operations:

 - `GETS<n> dst, ptr, offset` which treats `ptr` like a C pointer of
   type (`char *`) and reads `n` bytes from byte offset `offset`.
   Like for arrays, possible values for `n` will be `1`,`2`,`4`, or
   `8`.
   
 - `SETS<n> ptr, offset, val` writes to these locations.
 
One important part here is that values stored inside these stack
structures are not interpreted by the GC. This means, that if these
values actually refer to objects on the Haskell heap (as is the case
for GMP), then GC must not be possible during the lifetime of the
structures.

If these stack-allocated values are restricted to leaf functions, then
we can use a scratch pad and don't need to keep a separate stack (or a
separate stack frame area).

Example bytecode implementation:

    integer_cmm_plusInteger_bc:
      r0 : Int#
      r1 : Ref[ByteArray#]
      r2 : Int#
      r3 : Ref[ByteArray#]
    entry:
      FRAME     6
      ALLOCS    r4, 56 ; 3 * (sizeof(mpz_t) = 16) + 1 word (8 bytes)
      ; NOTE: No allocating instructions allowed from this point on.
      
      ; init tmp1
      ARR_SIZE  r5, r1       ; r5 = r1->size
      SETS4     r4, 0, r5    ; tmp1->alloc = r5
      SETS4     r4, 4, r0    ; tmp1->size = r0
      ARR_BYTES r1, r1       ; r1 = &r1->bytes
      SETS8     r4, 8, r1    ; tmp1->d = r1
      
      ; init tmp2
      ARR_SIZE  r5, r3       ; r5 = r3->size
      SETS4     r4, 16, r5   ; tmp2->alloc = r5
      SETS4     r4, 20, r2   ; tmp2->size = r2
      ARR_BYTES r3, r3       ; r3 = &r3->bytes
      SETS8     r4, 24, r3   ; tmp2->d = r1
      
      ; init result, with 1 limb stored right next to it on the stack
      LOADK     r0, 1
      LOADK     r1, 0
      SETS4     r4, 32, r0   ; result->alloc = 1
      SETS4     r4, 36, r1   ; result->size = 0
      ADDPTR    r2, r4, 48   ; r2 = pointer into scratch mem
      SETS8     r4, 40, r2   ; result->d = r2

      ADDPTR    r5, r4, 16   ; r5 = &tmp2
      ADDPTR    r6, r4, 32   ; r6 = &result
      LOADK     r0, FFI[__gmpz_add(ptr, ptr, ptr)]
      CCALL     r0, r6, r4, r5  ; call __gmpz_add(&result, &tmp1, &tmp2)

      GETS4     r0, r4, 36   ; r0 = result->size
      
      LOADK     r1, 0
      IFNE      r0, r1 ->L1  ; if r0 != 0 goto L1
      LOADK     r0, 0
      LOADK     r1, dummy_byte_array
      LOADK     r2, 0
      RETN      3            ; return (r0, r1, r2)
      
     L1:
      LOADK     r1, 1
      IFGT      r0, r1 ->L2  ; if r0 > 1 goto L2
      LOADK     r1, -1
      IFLT      r0, r1 ->L2  ; if r0 < -1 goto L2
      LOADK     r1, dummy_byte_array
      GETS8     r2, r4, 40   ; r2 = result->d
      GETS8     r2, r2, 0    ; r2 = r2[0]  -- XXX
      RETN      3            ; return 

     L2:
      GETS8     r1, r4, 40   ; r1 = result->d
      ARR_UNBYTES r1, r1     ; r1 = r1 - sizeof(ByteArray)
      LOADK     r2, 0
      RETN      3

The line marked with `XXX` is a bit fishy because it assumes that
`GETS8` is simply C array access. Perhaps that's what these
temporaries should use, though.  The `ADDPTR` instruction is basically
just `ADD`, but if we want to have a mode with bounds checks turned on
it makes sense to use a different instruction.

Given the current implementation, it would be simplest to have a
single scratchpad, which is reservered using `FRAME regs, bytes` and
which starts after the virtual registers. A pointer into the scratch
region is loaded into a virtual register via `SCRATCH dst, regs,
offset`, where `regs` must match the argument from `FRAME`. The
downside is that this won't work with sliding window.

On another note, if we can use a custom calling convention, we can
make this about as efficient as GHC's C-- wrappers. We expose it as a
C function and use the standard calling convention on x64:

    arg0 = rdi       ret0 = rax
    arg1 = rsi       ret1 = rdx
    arg2 = rdx       NEW: ret2 = rdi
    arg3 = rcx            ret3 = rsi
    arg4 = r8
    arg5 = r9
    
 



## Option 2: Heap as scratchpad

If we can guarantee that the code is uninterruptible, then we can also
use the heap.

    ALLOCSCRATCH r5, bytes
    ...
    <before return>
    FREESCRATCH r5, bytes  ; free memory we just allocated

This only works if no other allocations are happening in between and
if this is a leaf procedure. E.g., if we're calling out to C, this
must not call back into the Haskell RTS and allocate there (or it must
be a disjoint heap).

Of course, the safer alternative would be to just allocate a ByteArray
and rely on the copying GC to make deallocation cheap. It will cause
more allocation and hence more frequent GC, but it would work.

