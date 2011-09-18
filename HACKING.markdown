## Building

    cp mk/build.mk.sample mk/build.mk
    make boot
    make
    make check  # run test suite

If you changed the compiler and things don't rebuild as they should,
use `make clean`.  Rebuilding is quite fast at the momment, so this
shouldn't be too inconvenient.

If you changed the bytecode format (see below) then you also need to
do `make clean-bytecode`.

If you prefer to have actually useful error messages you can use
`clang` instead of `gcc` to build the C sources.  Call `make` with

    make CC=clang

or add `CC=clang` to your `mk/build.mk`.


## Changing the Bytecode

If you changed the bytecode binary format, at the very least you need
to check these files:

  - `rts/Bytecode.h`
  - `rts/Loader.c` 
  - `rts/Interp*.c`
  - `compiler/Lambdachine/Serialise.hs`

If you change the bytecode semantics, you have to check pretty much
everything.


## Debugging a Test

The test suite is based on the `lit` tool from LLVM.  Lit scans
directories for tests to run by looking inside the files.  Each file
contains special comments which describe how to run the test.

(TODO: Explain usage.  For now, just copy and adapt existing test
cases.)

If a test fails you can use re-run by supplying extra options to `make
check`.  For example, if test `Bench/Primes.hs` failed:

    make check TESTS=Bench/Primes.hs LITARGS=-v

If you get a segfault you may want to run the test under GDB.  To do that
first run the above command line to make sure that all the bytecode files
exist and are up to date.  Then run GDB:

    $ gdb ./interp
    # now we're in GDB, supply the necessary options
    > args -Btests Bench.Primes
    > run

The `-B` option specifies the search paths for the bytecode loader.
Note how we're specifying a module name, and not a file name as the
argument.  (TODO: it would be nice to allow both.)  You may want to
increase your debug output level by editing your `mk/build.mk`.

It's probably good to know that you can run `make` from within GDB.
My debugging workflow therefore usually looks as follows:

    # load gdb as above
    > run
    # inspect crash and edit source files
    > make
    > run
    # repeat
