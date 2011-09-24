{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0001 where

import GHC.Base
import GHC.Integer
import GHC.Num

allocList :: Int -> [Int]
allocList (I# n) = go n []
 where
   go 0# acc = acc
   go n# acc = go (n# -# 1#) (I# n# : acc)

len :: [Int] -> Int
len l = go 0# l
 where
  go acc# [] = I# acc#
  go acc# (_:l') = go (acc# +# 1#) l'

test = len (allocList 100) == I# 100#