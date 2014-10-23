{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bench.SumFromTo3 where

import GHC.Prim
import GHC.Types
import GHC.Base hiding (plusInt, eqInt, timesInt)

{-# NOINLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo from@(I# m) to@(I# n) =
  if isTrue# (m ># n) then [] else
    from : enumFromTo (I# (m +# 1#)) to

sum :: [Int] -> Int
sum l = sum_aux (I# 0#) l

{-# NOINLINE sum_aux #-}
sum_aux :: Int -> [Int] -> Int
sum_aux !acc [] = acc
sum_aux !(I# a) (I# x:xs) = sum_aux (I# (a +# x)) xs

-- Redefine here so it gets inlined even at lowest optimisation level
timesInt :: Int -> Int -> Int
timesInt (I# m) (I# n) = I# (m *# n)

plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)

succInt :: Int -> Int
succInt (I# m) = I# (m +# 1#)

eqInt :: Int -> Int -> Bool
eqInt (I# m) (I# n) = isTrue# (m ==# n)

zero :: Int
zero = I# 0#

one :: Int
one = I# 1#

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = let !y = f x in y : map' f xs

{-# NOINLINE root #-}
root upper =
  let !l = sum (map' (plusInt one) (enumFromTo zero upper)) in
  let upper' = succInt upper in
  (l `plusInt` l) `eqInt` (upper' `timesInt` (succInt upper'))

test = root (I# 100#)
