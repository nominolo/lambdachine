# Building

You need:

  - A recent [Haskell Platform][hp].
  - A C compiler, either `gcc` or `clang`
  
[hp]: http://hackage.haskell.org/platform/

Most dependencies are on Hackage and can be installed via `cabal`,
however, Hoopl's version on Hackage currently doesn't build with
GHC 7, so you have to build that by hand:

    $ cd some/temporary/directory
    $ git clone http://ghc.cs.tufts.edu/hoopl/hoopl.git
    $ cd hoopl
    $ cabal install
    
Now go back to the `lambdachine` directory and run:

    $ ./boot
    $ make install-deps
    $ make boot
    $ make

This should build the bytecode compiler `lcc` and the runtime
executable, currently called `interp`.  To run a benchmark use

    $ make bench2

See the `Makefile` for more targets.  The benchmarks are in directory
`tests/Bench/`.
