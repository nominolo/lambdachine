# Lambdachine

Lambdachine is virtual machine for Haskell which uses a trace-based
JIT compiler.  Lambdachine consists of two components:

  - `lcc`: A bytecode compiler that uses GHC to compile Haskell to
    Lambdachine bytecode files.

  - `lcvm`: The virtual machine which loads bytecode files and
    executes it.

## Caveats

Lambdachine currently only supports the pure subset of Haskell.
User-defined datatypes and operations on integers and characters are
supported, but mutable references, arrays, or I/O operations are not
yet supported.  The runtime system is single-threaded and no
concurrency primitives are supported.

## Building

You need:

  - GHC 7.8 compiled with `integer-simple` for integers
  - A C++ compiler, either `g++` or `clang`

All dependencies are on Hackage and can be installed via `cabal`,
  
Installation steps:

 1. Configuring

        $ ./boot   # Creates ./configure script
        $ ./configure   # Autodetect ghc, ghc-pkg, g++, etc.

    This will only find binaries in your `$PATH`.  You can specify a
    specific version of `ghc` and a matching `ghc-pkg` by passing them
    as arguments to the configure script, e.g.,

        $ ./configure --with-ghc=${HOME}/code/ghc/head/inplace/bin/ghc-stage2 \
                      --with-ghc-pkg=${HOME}/code/ghc/head/inplace/bin/ghc-pkg

 2. Building

        $ make install-deps
        $ make boot
        $ make

This should build the bytecode compiler `lcc`, the test suite and the
actual VM, called `lcvm`.  To run the test suite use:

    $ make check

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
