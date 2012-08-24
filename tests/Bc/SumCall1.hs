{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.SumCall1 where

import GHC.Bool
import GHC.Prim
import GHC.Types
import GHC.Base

{-# NOINLINE f #-}
f :: Int -> Int -> Int
f x y = (x `timesInt` 3) `plusInt` y

loop :: Int -> Int -> Int
loop 0 acc = acc
loop n acc =
  let !acc' = f n acc
      !n' = n `minusInt` 1
  in loop n' acc'

test = case loop 50 0 of
         I# n -> n ==# 3825#
