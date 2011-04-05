Memo 8: Biased Pointers
=======================

This is just a quick note on an implementation technique used by
LuaJIT.  The problem it is trying to solve is this:

 1. We have values that act like pointers, but only require, say, 16
    bits.  For example, they are references into an array of items.

 2. We conceptually have both positive and negative references.

 3. We want to be have "NULL" be a special value, so that we can use
    standard C `if` statements, e.g.,

        if (ref) { ... }

    rather than having to write:

        if (ref != REF_NULL) { ... }

    This can help to avoid errors.

LuaJIT uses **biased references** for this purpose:

    typedef uint16_t Ref;  // The type of references

    thing_t *buffer;  // A biased buffer (see below)
    Ref ref_lo, ref_hi;

    enum {
      REF_BIAS = 0x8000
    };

The idea is that a reference is always offset (biased) by `REF_BIAS`.
For example, the reference `+3` is represented as `REF_BIAS + 3`, so
in this particular case `0x8003`.  Negative references work the same
way, so `-4` is `REF_BIAS - 4` or `0x7ffc`.  Our 16 bit references
therefore have a value range of

    { NULL } + { -32767 ... +32767 }

and we have `NULL = 0`.  To avoid errors when using a buffer which is
indexed by such references, the buffer is biased as well.  To create a
buffer with the range `ref_lo` to `ref_hi`, we use this function:

    thing_t *allocBuffer(Ref ref_lo, Ref ref_hi)
    {
        assert(ref_lo < ref_hi);
        uint32_t size = (uint32_t)ref_hi - (uint32_t)ref_hi + 1;
        thing_t *buf = malloc(size * sizeof(thing_t));

        // Return the biased buffer.  The intuition is that we want
        // &biased_buf[ref_lo] == &buf[0];
        return (buf - ref_lo);
    }

Naturally, we should keep `ref_lo` and `ref_hi` around for bounds
check purposes.
