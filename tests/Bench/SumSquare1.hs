{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bench.SumSquare1 where

import GHC.Prim
import GHC.List
import GHC.Base
import GHC.Num

enumFromTo'Int :: Int -> Int -> [Int]
enumFromTo'Int from@(I# m) to@(I# n) =
  if m ># n then [] else
    from : enumFromTo'Int (I# (m +# 1#)) to

sum :: [Int] -> Int
sum l = sum_aux (I# 0#) l

{-# NOINLINE sum_aux #-}
sum_aux :: Int -> [Int] -> Int
sum_aux !acc [] = acc
sum_aux !(I# a) (I# x:xs) = sum_aux (I# (a +# x)) xs

{-# NOINLINE root #-}
root :: Int -> Int
root x = sum [ I# (a# *# b#) 
             | a@(I# a#) <- enumFromTo'Int 1 x
             , I# b# <- enumFromTo'Int a x ] 

test = root 10 == 1705
