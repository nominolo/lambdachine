{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Bc0011 where

import GHC.Base
import GHC.Integer
import GHC.Num
import GHC.Classes

fib :: Int -> Int
fib n | n <= 1 = 1
      | otherwise = fib (n - 2) + fib (n - 1)

test = fib 5 == 8
