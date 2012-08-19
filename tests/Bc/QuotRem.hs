{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.QuotRem where

import GHC.Bool
import GHC.Prim
import GHC.Types

-- import Prelude
-- import Debug.Trace

mygcd :: Int# -> Int# -> Int#
--mygcd x y | trace "..." False = 0#
mygcd x 0# = x
mygcd x y = mygcd y (x `remInt#` y)

{-
mylcm :: Int# -> Int# -> Int#
mylcm _ 0# = 0#
mylcm 0# _ = 0#
mylcm x y = (x `quotInt#` mygcd x y) *# y
-}

loop :: Int# -> Int# -> Int
loop acc s =
--  trace (show (I# acc, I# s)) $
  if s <=# 0# then I# acc else
    let !g = mygcd acc s in
--    trace (show (I# g)) $
    loop (acc +# mygcd acc s) (s -# 3#)

test = case loop 0# 100# of
         x@(I# r) -> r ==# 133#


test1 = I# (mygcd 12# 15#) -- 3
--test2 = I# (mylcm 12# 15#) -- 60