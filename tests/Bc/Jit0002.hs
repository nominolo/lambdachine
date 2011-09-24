{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0002 where

import GHC.Base
import GHC.Integer
import GHC.Num

allocList :: Int -> [Int]
allocList (I# n) = go1 n [] []
 where
   go1 0# acc1 _acc2 = acc1
   go1 n# acc1 acc2 =
     go1 (n# -# 1#) (I# n# : acc2) acc1
--   go2 n# acc1 acc2 =
--     go1 n# (I# n# : acc2) acc1 -- swapped arguments

len :: [Int] -> Int
len l = go 0# l
 where
  go acc# [] = I# acc#
  go acc# (_:l') = go (acc# +# 1#) l'

test = len (allocList 50) == I# 25#

{-

Goal:
 - PHI x, y
 - PHI y, x

-}