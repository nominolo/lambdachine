{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
module Bc.SparseCase where

import GHC.Prim
import GHC.Base

data X = A | B | C | D | E | F | G | H | J | K | L

{-# NOINLINE f1 #-}
f1 :: X -> Int#
f1 G = 5#
f1 _ = 4#

{-# NOINLINE f2 #-}
f2 :: X -> Int#
f2 A = 10#
f2 _ = 20#

{-# NOINLINE f3 #-}
f3 :: X -> Int#
f3 L = 100#
f3 _ = 200#

{-# NOINLINE f4 #-}
f4 :: X -> Int#
f4 D = 1000#
f4 K = 2000#
f4 _ = 6000#

test =
  (f1 G +# f1 A +# f2 A +# f2 E +# f3 B +# f3 L +# f4 J  +# f4 D  +# f4 K) ==#
  (5#   +# 4#   +# 10#  +# 20#  +# 200# +# 100# +# 6000# +# 1000# +# 2000#)
