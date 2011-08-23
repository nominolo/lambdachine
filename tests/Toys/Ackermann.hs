{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.I#`con_info 13
module Toys.Ackermann where

import GHC.Num
import GHC.Base
import GHC.Integer

ack :: Int -> Int -> Int
ack m n = --trace "." $
  if m == 0 then n + 1 else 
    if n == 0 then ack (m-1) 1 else
      ack (m-1) (ack m (n-1))

root x = ack 3 x

test = root 1
