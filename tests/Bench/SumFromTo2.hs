{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bench.SumFromTo2 where

import GHC.Prim
import GHC.Bool
import GHC.Types
import GHC.Base
import GHC.Num

{-# NOINLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo from@(I# m) to@(I# n) =
  if m ># n then [] else
    from : enumFromTo (I# (m +# 1#)) to

{-# NOINLINE sum #-}
sum :: (Num a) => [a] -> a
sum     l       = sum' l 0
  where
    sum' []     !a = a
    sum' (x:xs) !a = sum' xs (a+x)

{-# NOINLINE root #-}
root :: Int -> Int
root x = sum (enumFromTo 1 x)

test = root (I# 100#)
