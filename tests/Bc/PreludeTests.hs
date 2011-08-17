{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.I#!con_info 0x000000000000000a
module Bc.PreludeTests where

import GHC.Base
import GHC.Num

t1 :: Int -> Int -> Bool
t1 x y = x /= y

test = test4

test1 = let !b = t1 3 4 in
  if b then t1 (I# 5#) (I# 5#) else False

test2 :: Int
test2 = 3 - 11 + 6 * 8 - (-2)

rep :: Int# -> [Int]
rep n = if n ==# 0# then [] else I# n : rep (n -# 1#)

--test = rep 3#

{-# NOINLINE app #-}
app :: (a -> b) -> a -> b
app f x = f x

test3 :: Bool
test3 = if (app eqInt 3) (4 :: Int) then False else True

sum :: Int -> [Int] -> Int
sum n [] = n
sum !n (x:xs) = sum (n + x) xs

test4 :: Int
test4 = sum 0 (rep 4#)

test5 :: Int
test5 = sum 0 [1,2]
