{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bench.SumFromTo1 where

import GHC.Prim
import GHC.Bool
import GHC.Types

{-# NOINLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo from@(I# m) to@(I# n) =
  if m ># n then [] else
    from : enumFromTo (I# (m +# 1#)) to

zero :: Int
zero = I# 0#

one :: Int
one = I# 1#

sum :: [Int] -> Int
sum l = sum_aux (I# 0#) l

{-# NOINLINE sum_aux #-}
sum_aux :: Int -> [Int] -> Int
sum_aux !acc [] = acc
sum_aux !(I# a) (I# x:xs) = sum_aux (I# (a +# x)) xs

timesInt :: Int -> Int -> Int
timesInt (I# m) (I# n) = I# (m *# n)

plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)

succInt :: Int -> Int
succInt (I# m) = I# (m +# 1#)

eqInt :: Int -> Int -> Bool
eqInt (I# m) (I# n) = m ==# n

{-# NOINLINE root #-}
root upper =
  let !l = sum (enumFromTo one upper) in
  (l `plusInt` l) `eqInt` (upper `timesInt` (succInt upper))

test = root (I# 100#)
