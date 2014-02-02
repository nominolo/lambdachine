{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc
module Test.Basic0003 where

import GHC.Prim

-- Allocate a data object (not a thunk)
data X = X Int# X | Y

f :: Int# -> X
f n = X n (X n Y)
