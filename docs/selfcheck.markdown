Self-check Mode
===============

In order to verify the optimised machine code we run it against the
interpreter (which serves as a reference implementation).  The basic
approach to do this is as follows:

The machine code is run on a shadow environment (stack and heap).  We
run the machine code until it reaches a checkpoint or falls back to
the interpreter on its own.  We then run the interpreter on the actual
heap and stack and report any deviations as we detect them.

This gives rise to a number of design decisions explored in the
following:


## Shadow Stack

Since we are not that much interested in performance, we can just copy
the whole stack.  It might make sense to have a low-water mark to
minimise the number of items we have to compare.  Base pointer and top
pointer need to be replaced by shadow versions.


## Shadow Heap

There are three operations we perform on the heap: reading a value,
allocating a new object, and modifying an existing object.  Allocating
new objects can be done by letting the machine code allocate into a
different block (arena).  Modifying an existing object can take one of
two forms -- changing the value of a mutable object (IORef, IOArray,
etc.) or updating a thunk.

The update operation can be handled by writing to a log and then
checking that log whenever following an indirection.  After the
compiled code has stopped the log is used to reset all updated
thunks.  Whenever the interpreter performs an update operation
it should add its value to the log, too.


The shadow heap should behave roughly like a hash table.  Whenever a
value is written to the heap it is instead written to the shadow heap.
Whenever a value is read from the heap we first look it up in the shadow
heap first and look it up in the real heap second.  Heap pointer and
heap limit need to be replaced by shadow versions.

This means we need different implementations for heap loads and stores
depending whether we're in verification mode or not.  This code should
be kept as simple as possible -- using a full hash table seems
complex.

TODO: We really only need the hash table for the verification part, so
just allocating to a different block should be fine.  The verification
code can then be a little more complex.


## Where to check and how?

If we disallow allocation sinking, then consistency requires
byte-for-byte equality.  I.e., even allocation order should be same.
If we allow allocation sinking or reordering of instructions in the
trace, then we consistency requires isomorphic heap closures.  Formally: 

  * Let `lives(S)` be the transitive closure of all heap nodes
    reachable from live variables on the stack `S`.  The shadow heap
    is consistent with the real heap if and only if there exists a
    bijection between `lives(shadow stack)` and `lives(real stack)`.
    This implies that the bijection must also hold for the fields of
    all live objects on the heap.  In addition, the contents of all
    live non-pointer registers must be identical.

We only need to verify this property for the newly-written values of
the shadow heap.  It holds trivially for the unmodified part of the
heap.  We verify this by computing the bijection, which can be done
using the following algorithm:

    Let Iso = {}
    Let Todos = {}
    Foreach n in pointer registers on the stack:
      p = ShadowStack(n)
      q = RealStack(n)
      if IsOnShadowHeap(p):
        if defined Iso(p):
	  if Iso(p) /= q:
            fail  -- inconsistency found
        else:
          Iso(p) = q
          Todos += { p }

    -- now do the same for Todos until Todos is empty (i.e., verify
    -- the transitive heap closure until we hit the shared heap)

    -- TODO: This does not handle updates

