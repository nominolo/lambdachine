{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0006 where

import GHC.Base
import GHC.Integer
import GHC.Num

--import Debug.Trace
--import Prelude (show)

-- Tests register allocator for handling of sunken objects

loop :: Int -> Int -> Int -> Int
--loop x y z | trace (show (x,y,z)) False = let a = a in a
loop (I# 0#) (I# x) (I# y) = I# (x +# y)
loop (I# n) (I# x) y = loop (I# (n -# 1#)) (I# (x +# n)) (I# x)

test = loop 15 3 4 == 245