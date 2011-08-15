{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bc0008 where

import GHC.Prim
import GHC.Types
import GHC.Bool

test = test3

plusInt :: Int -> Int -> Int
plusInt (I# n) (I# m) = I# (m +# n)

gtInt :: Int -> Int -> Bool
gtInt (I# m) (I# n) = m ># n

upto :: Int -> Int -> [Int]
upto !from !to =
  if from `gtInt` to then []
   else from : upto (from `plusInt` I# 1#) to

sum :: Int -> [Int] -> Int
sum n [] = n
sum (!n) (x:xs) = sum (n `plusInt` x) xs

test1 = sum (I# 0#) []
test2 = sum (I# 0#) [I# 1#, I# 2#]
test3 = sum (I# 0#) (upto (I# 1#) (I# 10#))
