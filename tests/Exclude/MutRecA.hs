{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module MutRecA where

import GHC.Prim

import {-# SOURCE #-} MutRecB

data A = A Int# B

f :: A -> Int#
f (A n _) = n
