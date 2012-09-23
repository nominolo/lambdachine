# Building

You need:

  - GHC 7.0.* (newer versions not yet supported)
  - A C compiler, either `gcc` or `clang`
  
All dependencies are on Hackage and can be installed via `cabal`,
  
Installation steps:

    $ ./boot
    $ ./configure
    $ make install-deps
    $ make boot
    $ make

This should build the bytecode compiler `lcc`, the test suite and the
actual VM, called `lcvm`.  To run the test suite use:

    $ make test

See `Makefile.in` for more targets.  The benchmarks are in directory
`tests/Bench/`.  Benchmarks use the C preprocessor to compile a
version for GHC and another for Lambdachine.  For example, to build
the benchmark `SumFromTo1` use:

    $ make bench-ghc/SumFromTo1
    $ ./bench-ghc/SumFromTo1 +RTS -s -A1m    # run it
    $ make tests/Bench/SumFromTo1.lcbc     # also built by "test" target
    $ ./lcvm -e bench Bench.SumFromTo1

For benchmarking, make sure to build `lcvm` with full optimisations.
To do so, edit your `mk/build.mk` file to contain the following line:

    EXTRA_CXXFLAGS=-O3 -DNDEBUG

Each benchmark has code for two versions: a test version and a
benchmark version. By default, the `test` target is used.  To run a
benchmark, you have to explicitly specify to use `bench` as the entry
point.  This is what the `-e` option does.
