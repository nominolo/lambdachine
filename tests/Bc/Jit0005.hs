{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0005 where

import GHC.Base
import GHC.Integer
import GHC.Num

-- Tests whether heap overflow checks are handled correctly.
-- Ideally, the parameters should be tuned to the block size.

inner :: Int# -> Int# -> Int#
inner 0# n = n
inner m  n = inner (m -# 1#) (m +# n)

outer :: Int# -> Int# -> Int
outer 0# acc = I# acc
outer m acc = outer (m -# 1#) (inner m acc)

test = outer 10# 0# == 220