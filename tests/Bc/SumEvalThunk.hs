{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.SumEvalThunk where

import GHC.Bool
import GHC.Prim
import GHC.Types

upto :: Int# -> Int# -> [Int]
upto lo hi =
  if lo ># hi then [] else
    I# lo : upto (lo +# 1#) hi

loop :: Int# -> [Int] -> Int#
loop acc [] = acc
loop acc (I# x : xs) = loop (acc +# x) xs

test = case loop 0# (upto 1# 100#) of
         res -> res ==# 5050#
