{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.SumDict where

import GHC.Bool
import GHC.Prim
import GHC.Types
import GHC.Num
--import Prelude

upto :: Int# -> Int# -> [Int]
upto lo hi =
  if lo ># hi then [] else
    I# lo : upto (lo +# 1#) hi

{-# NOINLINE loop #-}
loop :: Num a => a -> [a] -> a
loop acc [] = acc
loop acc (x : xs) =
  let !acc' = acc + x in loop acc' xs

test = case loop 0 (upto 1# 1000#) of
         a@(I# res) -> res ==# 500500#

--main = print test
