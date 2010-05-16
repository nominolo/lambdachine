# Status

Extremely unstable.  Currently, the only supported use case is:

    $ cd compiler
    $ ghci -package ghc -hide-package mtl -hide-package monads-tf Main.hs
    ghci> :set args ["../tests/sumfromto.hs"]
    ghci> main

You also need GHC 6.12 for this.
