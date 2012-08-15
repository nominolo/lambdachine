{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.EvalThunk where

import GHC.Bool
import GHC.Prim
import GHC.List
import GHC.Types
--import GHC.Base

{-# NOINLINE f #-}
f :: Int# -> [Int] -> Int#
f acc [] = acc
f acc (I# n : ns) =
  let !acc' = acc +# n in
  f acc' ns

upto :: Int# -> Int# -> [Int]
upto lo hi =
  if lo ># hi then [] else 
    let !lo' = lo +# 1# in I# lo : upto lo' hi

test = (f 0# (upto 1# 123#)) ==# 7626#
