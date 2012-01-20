{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %lcc %s && %interp %m --stack=64K | %filecheck %s
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0011 where

import GHC.Base
import GHC.Integer
import GHC.Num

-- Use a lot of stack.

loop :: Int# -> Int
loop 0# = I# 0#
loop n =
  case loop (n -# 1#) of
    I# a -> I# (a +# n)

test = loop 1000# == 500500
