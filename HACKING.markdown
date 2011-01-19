## Building

    make boot
    make
    make test4  # or whichever you want to run

If you changed the compiler and things don't rebuild as they should,
use `make clean`.  Rebuilding is quite fast at the momment, so this
shouldn't be too inconvenient.

If you changed the bytecode format (see below) then you also need to
do `make clean-bytecode`.


## Changing the Bytecode

If you changed the bytecode binary format, at the very least you need
to check these files:

  - `rts/Bytecode.h`
  - `rts/Loader.c` 
  - `rts/Interp*.c`
  - `compiler/Lambdachine/Serialise.hs`

If you change the bytecode semantics, you have to check pretty much
everything.
