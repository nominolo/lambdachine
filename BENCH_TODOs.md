Fibon
=====

We do *not* use the DPH benchmarks.  They are micro-benchmarks anyway.

Shootout
--------

 - binary trees: MAYBE - may need IO for printing, tests mostly allocation
 - chameneosredux: NO - (requires multithreading, MVars)
 - fannkuch: YES - IO for output (could be removed)
 - mandelbrot: MAYBE - uses Double, foreign ptr/array, handle IO
 - nbody: MAYBE - Double, IORef, foreign Ptr
 - pidigits: YES - Integer, IORef (avoidable), IO output (avoidable?)
 - spectralnorm: MAYBE - Double, foreign Ptr

Hackage
-------

  - Agum: YES - long input string, long expected output string, IO removable

  - Bzlib: NO - spends most of its time in C code

  - CPSA: MAYBE - huge, but seems to be mostly pure code.

  - Crypto: PARTS - we could derive sub-benchmarks for various crypto
    algorithms (e.g., SHA1). They seem to need Array-based lookup tables
    (e.g., accumArray) and bitops

  - Fgl: YES - Main benchmark seems to use patricia trees (need IntMap)
    graph parsing could probably be made to use string constants

  - Fst: NO - Array of non-primitive type

  - Funsat: NO - huge, and too many dependencies

  - Gf: NO - very large, many dependencies

  - HaLeX: YES - produces DFA, mostly pure  200 x repetitions 

  - Happy: NO - uses itself to generate its parser

  - Hgalib: YES - Double, pure, fairly small

  - Palindromes: YES - benchmark runs 120 repetitions of the
    LongestTextPalindrome query (on four files).  Needs arrays.

  - Pappy: YES - pure, parses Java files (pure parser)

  - QuickCheck: YES - separate out pure stuff; fibon runs 5 x 1000000 test cases

  - Regex: MAYBE/NO - includes Parsec, uses ByteString

  - Simgi: NO - uses FFI for mersenne PRNG

  - TernaryTrees: YES - read large string

  - Xsact: MAYBE - pattern matching;   90 x XSact -i pdbnt  (NoLCR)

Other Benchmarks
----------------

  - DPLL incremental-sat-solver, enumerate all solutions and check
    their validity

  - Map, IntMap, IntSet, Set: benchmarks

  - Safe random number splitter (from HIW)

  - SipHash


Nofib (Max's subset)
--------------------

  - bernouilli: YES - uses Integer

  - digitsofe2: YES - uses Integer, really slow when using integer-simple

  - exp3_8: YES (BROKEN) - very simple (and also quite useless)

  - primes: YES - very simple (and deeply flawed)

  - rfib: YES - uses Double, 

  - tak: YES - probably difficult for tracing JIT

  - x2n1: MAYBE - needs Double and Complex

  - queens: YES - nice and simple

  - append: YES (SLOW)

  - factorial: YES (non-tail-recursive)

  - raytracer: YES - it's just: sum'Int (zipWith (*) xs ys)

  - sumtree: YES (GC Crash)

  - treeflip: YES

  - sumsquare: YES: 

      sum'Int [ k * m | k <- enumFromTo'Int 1 n, m <- enumFromTo'Int 1 k ]
      root 10000 == 1400446540

  
