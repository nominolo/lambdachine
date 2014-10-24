Memo 3: Garbage Collection
==========================

## Copying GC and Generational GC

A copying GC is great for efficiently allocating small temporary
objects.  The work done by the GC is proportional to the number of
*surviving* objects.  Functional programming languages seem to have a
particularly low survival rate (see paper
["new Scala() instanceof Java"][scala-gc]).  A copying GC also moves
objects and thereby compacts and eliminates memory fragmentation.

For larger objects, or for objects that are kept around for a longer
period copying GC leads to too much unnecessary memory traffic.  We
can handle large objects by allocating them in different regions and
collect those regions using mark-sweep GC.  Long-lived objects are
typically handled by using using generational garbage collection.

Generational GC splits the heap into N generations, typically two or
three.  New objects are allocated into the first generation, called
"nursery" or "Eden".  Once the first generation has been filled up
only that generation is garbage collected and any surviving objects
are transferred ("promoted") into the next generation which is usually
called "mature space" or "tenured space".  If the second generation is
filled, both the first and the second generation are collected
together.  If there is a third generation, then both first and second
generation would also be collected when the third generation is
collected.

The *generational hypothesis* states that only a small percentage of
new objects survives long enough to reach the later generations.
Thus, the (smaller) nursery is collected much more frequently than the
(larger) mature space.  We can tune the size of the nursery to fit
into a processor's L2 cache to make sure there is little latency when
collecting it.  Note, though, that more frequent collection means that
objects have less time to die, which in turn means more objects get
promoted into the next generation causing more frequent GCs of that
generation.

Since the nursery must be collected independently from the mature
space, we must keep track of pointers from the mature space into the
nursery.  Objects in the nursery must be considered alive and when the
object is moved, all pointers to it must be updated.  Pointers from
mature space to objects in the nursery can only occur if an object in
the mature space is modified.  Therefore, generational GCs use a
*write barrier*, i.e., an extra check when writing into an object.  If
we store a pointer that we must keep track of, then some extra work is
done.

 [scala-gc]: www.lirmm.fr/~ducour/Doc-objets/ECOOP2012/ISMM/ismm/p97.pdf


## Managing Large Objects

While a copying GC works well for managing small objects with short
lifetimes, it really becomes too inefficient for larger objects.  Some
objects also cannot be moved, e.g., byte arrays that are passed to C
functions.

Our heap is divided into **regions** which often are sub-divided into
**blocks**.  Regions are currently 1MB and blocks are currently 32KB
large.  Small objects are allocated into blocks.  Blocks are used to
reduce fragmentation and memory overhead of copying garbage
collection.  The heap is segregated into multiple logical *spaces*
(e.g., nursery/eden space, mature space) which in turn are made up of
collections of blocks which need not be adjacent in memory.

Regions are aligned at multiple of their sizes.  Given a pointer to an
object we can find its region by just rounding its address down to the
next multiple of a region size.

Organising large objects into blocks would be wasteful as it would
limit the size of large objects and could lead to a lot of
fragmentation.  Instead we dedicate a whole region to contain only
large objects.  Large objects that exceed the size of a region are
allocated at region alignment and obtain a region header, but are
otherwise separate.

Large objects are collected using mark-sweep rather than copying.
Instead of evacuating (copying) an object we push its address onto a
mark stack and follow internal pointers later.

Large objects can be recognised by their header (instead of just the
region type they reside in), so it's easy for the GC to decide how to
collect an object.


 
