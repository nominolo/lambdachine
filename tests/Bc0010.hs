{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bc0010 where

import GHC.Base
import GHC.Integer
import GHC.Num

fib1 :: Int -> Int
fib1 0 = 1
fib1 1 = 1
fib1 2 = 2
fib1 3 = 3
fib1 4 = 5
fib1 5 = 8
fib1 n = fib1 (n - 2) + fib1 (n - 1)

isFourtyTwo :: Int -> Bool
isFourtyTwo 42 = True
isFourtyTwo _ = False


test =
  fib1 5 == 8 && isFourtyTwo 42