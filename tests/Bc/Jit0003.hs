{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0003 where

import GHC.Base
import GHC.Integer
import GHC.Num

--import Debug.Trace
--import Prelude ( show )

swap :: Int# -> Int# -> Int# -> Int
swap 0# n m = I# m
swap c n m = swap (c -# 1#) m n

test = (swap 15# 1# 2#) == 1