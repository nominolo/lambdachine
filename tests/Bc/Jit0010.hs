{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
-- Should cause a stack overflow with the current setup.
-- XFAIL: *
module Bc.Jit0010 where

import GHC.Base
import GHC.Integer
import GHC.Num

-- Cause a stack overflow on trace.

loop :: Int# -> Int
loop 0# = I# 42#
loop n = case loop (n -# 1#) of
           I# a -> I# (a +# 2#)

test = loop 10000# == 20042
