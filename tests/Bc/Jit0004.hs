{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0004 where

import GHC.Base
import GHC.Integer
import GHC.Num

-- Tests whether heap overflow checks are handled correctly.
-- Ideally, the parameters should be tuned to the block size.

mkList :: Int# -> [Int] -> [Int]
mkList 0# acc = acc
mkList c acc =
  let !acc' = I# c : acc in
  mkList (c -# 1#) acc'

check :: Int# -> [Int] -> Bool
check _ [] = True
check n (I# m : ls) =
  if n ==# m then check (n +# 1#) ls else False

test = check 1# (mkList 5000# [])