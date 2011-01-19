Memo 1: Interaction of EVAL/APPLY and calling convention
========================================================

Consider this Haskell expression (occurring in function `g`).

    g x = ... (f a b c d)

Assuming `f` is in tailcall position this translates into the
following bytecode:

    CALLT f a b c d

Assume for now that `a`, ..., `c` are allocated to registers `r0`
through `r3`, and `g` uses 7 local registers.  The stack of `g` before
the execution of `CALLT` will then look like this:

        +----+----+----+----+----+----+----+----+
    ... | g  |  a |  b |  c |  d |    |  f |    |
        +----+----+----+----+----+----+----+----+
             ^ r0   r1   r2   r3   r4   r5   r6 ^
             |                                  |
            base                               top

The following two sections assume that `f` is a function closure.
Other possible cases are discussed in Section "Partial application"
below.

## Exact application

If `f` has arity 4 then we will reuse the current stack frame and no
arguments need to be copied (because we're doing a tail-call).  Only
`g` gets copied into the position of the `Node` pointer (`base[-1]`).
If `g` has a different frame size than `f`, the `top` pointer is moved
as well.  For example, if `f` has frame size 6, the stack will now
look as follows:

        +----+----+----+----+----+----+----+
    ... | f  |  a |  b |  c |  d |    |    |
        +----+----+----+----+----+----+----+
             ^ r0   r1   r2   r3   r4   r5 ^
             |                             |
            base                          top

If `f` is a known function, we can statically ensure that we will
always end up in this case.


## Overapplication

Now assume that `f` has arity 2, i.e., `f` is *overapplied*.

In this case we:

 1. Create a stack frame for `f`.

 2. Copy the first two arguments into the stack frame.

 3. Copy the two remaining arguments `c` and `d` *down* into registers
    `r0` and `r1`.

 4. Change the node pointer (`g`) into `stg\_AP2\_closure` and set the
    return address of the newly-allocated stack frame to point to the
    first instruction of the `stg_AP2_closure` code.

When overapplying a function with arity *N* call the first *N*
arguments the *immediate arguments*, and the remaining arguments the
*extra arguments*.

After step 2 our stack will look as follows:

             .------------------------------------.
             v                                    |
        +----+----+----+----+----+----+----+----+-*-----+----+----+----+- ... -+
    ... | g  |  a |  b |  c |  d |    |  f |    | Frame |  f |  a |  b |       |
        +----+----+----+----+----+----+----+----+-----*-+----+----+----+- ... -+
             ^ r0   r1   r2   r3   r4   r5   r6 ^     |     ^                  ^
             |                                  |     :     |                  |
            base                               top    v    base'            base'+6

The `Frame` part contains the return address and a pointer to the
old stack frame base.

After step 4 the stack frame will now look like this:

             .------------------------------------.
             v                                    |
        +----+----+----+----+----+----+----+----+-*-----+----+----+----+- ... -+
    ... | *  |  c |  d |    |    |    |    |    | Frame |  f |  a |  b |       |
        +-|--+----+----+----+----+----+----+----+-----*-+----+----+----+- ... -+
          |  ^ r0   r1   r2   r3   r4   r5   r6 ^     |     ^                  ^
          |  |                                  |     |     |                  |
          | base                               top    |   base'            base'+6
          v                                           v
         stg_AP2_closure                          stg_AP2_code

This setup means that once `f` returns, execution will continue at the
code for `stg_AP2_code` and will reuse the stack frame that originally
belonged to `f`.  The code for `stg_AP2_code` looks as follows:

    MOV_RES r2        ;  r2 = result of last call or eval
    CALLT   r2 r0 r1  ;  return r2(r0, r1)

Once `f` returns (and the result must be a function) execution will
continue at the beginning of this code and apply the remaining two
arguments.

One might ask whether we really need to copy down extra arguments
into the lower N registers.

If we did not do this, we would need different versions of `AP2` for
each possible arity of functions, which may be quite a large number.
Furthermore, for efficiency reasons we require that arguments to tail
calls are allocated in registers `r0`, `r1`, etc (in order).  That
means that the code for `AP2` in our above example would have to look
as follows:

    MOV     r0 r2     ; r0 = r2
    MOV     r1 r3     ; r1 = r3
    MOV_RES r2        ; r2 = result of last call or eval
    CALLT   r2 r0 r1  ;  return r2(r0, r1)

You can see, we have to copy the results down anyway; and doing it in
the implementation of `CALLT` is faster than in two separate bytecode
instructions.

There is a real downside of this implementation, though.  The stack
frame of `stg_AP2_closure` only requires three slots while the stack
frame of the caller (`g` above) is guaranteed to be larger.  It would
be nice to resize the caller's stack frame before creating the stack
frame for `f` to avoid wasting stack space.

This also means that the following invariant does *not* hold (assuming
a frame size of 2):

  * When returning from a function, `top = base - 3`.

We need the `top` pointer whenever we allocate stack frames to check
for stack overflows and to find out where to allocate the next stack
frame.  This can be quite frequent: `CALL`, `CALLT` and `EVAL` all
allocate stack frames, and they are all very common.  The `top`
pointer can be calculated from `base` by reading the meta-information
from the code section of the current `Node` pointer, but that requires
following two pointers.  Using the above formula we can store away the
`top` pointer in the thread state and update it whenever we execute a
call, eval or return instruction.

These drawbacks can be avoided by copying one part of the arguments
twice.  Either:

  * Copy the immediate arguments to a scratch area, then copy down the
    extra arguments, and finally copy the immediate arguments from the
    scratch area into the new stack frame.  Or:

  * Copy the extra arguments into the scratch area, then copy the
    immediate arguments into the new stack frame, and finally copy the
    extra arguments into the old stack frame.

We may even dynamically choose between either strategy based on which
set is smaller.


### Built-in Closures

The above implementation method requires one variant of
`stg_APn_closure` for every possible `n` where the possible of values
of `n` are application dependent.  We have a number of options to
handle this:

 1. Add a new bytecode instruction that takes the number of arguments
    to apply from some fixed location in the current stack frame.
    This may create issues with stack frame layout (e.g., describing
    which variables are live, etc.).

 2. Dynamically create the closure if needed.  This adds very little
    extra complexity since runtime loading of code is a planned
    feature anyway.

 3. Create versions of a fixed number set of values for `n`, say, 1
    through 8.  This means that our code can contain `CALL`
    instructions with up to 9 arguments.

    What about calls with more than 9 arguments?  Simple!  Just turn
    them into two calls (the first of which will yield a partial
    application).  For example for a call with 14 arguments:

        CALL f x1 ... x9
        MOV_RES g
        CALL g x10 ... x14

    Since calls with more than 8 arguments are very rare in practise,
    this is a reasonable trade-off.

Options (2) and (3) should both be reasonably easy to implement, in
fact, they can be combined.  Option (3) would also mean that the
encoding for `CALL` instructions could have a small maximum bound on
the number of arguments.  It's not clear whether this is a useful
feature to have.


## Partial application

Now assume that `f` has arity 6, i.e., `f` is *partially applied*.

In this case we:

  1. Allocate a PAP closure ("PAP" = "partial application").  In this
     closure we store (a) the number of remaining arguments, (b) a
     pointer to the closure for `f`, and (c) all arguments.

  2. Put a pointer to this closure into the result register, so that
     the next `MOV_RES` instruction will receive this function as a
     result.

  3. Return to the parent.

So, creating a PAP is simple, but what happens if a `PAP` occurs in the
function position of a `CALL` instruction?

If there are still arguments missing we just create new PAP which
contains the contents of the old PAP and the new arguments.
Otherwise, we now have to call the function from the PAP.  The tricky
bit is, again, the stack frame management.

Assume our PAP pointed to by `f` has the shape

    f -> PAP 2 g x y

and our call site looks like this

    CALL f a b

We now set up a new stack frame for `g` that looks as follows:

        +----+----+----+----+----+--     --+
    ... | f  |  x |  y |  a |  b |   ...   |
        +----+----+----+----+----+--     --+
             ^ r0   r1   r2   r3           ^
             |                             |
         new base                        new top

This can be achieved by first copying the arguments from the PAP into
the new stack frame and then the arguments from the CALL.

For a tail call, we can reuse the current stack frame.  Only this
time we first have to move the CALL args into place and then copy the
PAP args.

Of course, we may overapply a PAP in which case we have to set up the
application stack frames as discussed above.
