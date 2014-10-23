{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.SumDict where

import GHC.Prim
import GHC.Types
import GHC.Num
--import Prelude

upto :: Int# -> Int# -> [Int]
upto lo hi =
  if isTrue# (lo ># hi) then [] else
    I# lo : upto (lo +# 1#) hi

{-# NOINLINE loop #-}
loop :: Num a => a -> [a] -> a
loop acc [] = acc
loop acc (x : xs) =
  let !acc' = acc + x in loop acc' xs

test = case loop 0 (upto 1# 1000#) of
         a@(I# res) -> isTrue# (res ==# 500500#)

--main = print test
