{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.True`con_info
module Test.Small0001 where

import GHC.Prim
import GHC.Types

{-# NOINLINE g #-}
g :: Int -> Int -> Bool
g (I# n) (I# x) = isTrue# (n /=# x)

test = g 6 4
