{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
module Bc.Basic0004 where

import GHC.Prim

data Int = I# Int#

-- Allocate a data object (not a thunk)
data X = X Int X | Y

f :: Int# -> X
f n = X (g n) Y

{-# NOINLINE g #-}
g :: Int# -> Int
g n = I# (n +# n)

h = g 5#

test = h

