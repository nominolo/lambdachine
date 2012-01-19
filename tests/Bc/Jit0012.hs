{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0012 where

import GHC.Base
import GHC.Prim
import GHC.Num

-- allocate lots of partial applications

{-# NOINLINE f #-}
f :: Int -> Int -> Int -> Int -> Int
f a b c d = a - b + c * d

loop1 :: Int# -> [Int -> Int -> Int -> Int] -> [Int -> Int -> Int -> Int]
loop1 0# acc = acc
loop1 n acc =
  loop1 (n -# 1#) (f (I# n) : acc)

loop2 :: Int# -> [Int -> Int -> Int -> Int] -> [Int -> Int]
loop2 n [] = []
loop2 n (g:gs) =
  let !nn = I# n in
  g nn nn : loop2 (n +# 1#) gs

asum :: Int# -> [Int -> Int] -> Int
asum acc [] = I# acc
asum acc (h:hs) =
  case h 7 of
    I# h' -> asum (acc +# h') hs

test1 = asum 0# (loop2 3# (loop1 1000# []))

test = test1 == 3515500
