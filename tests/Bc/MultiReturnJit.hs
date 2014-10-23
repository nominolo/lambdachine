{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}
module Bc.MultiReturnJit where

import GHC.Prim
import GHC.Types
import GHC.Num
import GHC.Base

{-# NOINLINE sumlen #-}
sumlen :: [Int] -> (# Int, Int #)
sumlen [] = (# 0, 0 #)
sumlen (I# x : xs) =
  case sumlen xs of
    (# I# s, I# l #) -> (# I# (s +# x), I# (l +# 1#) #)

{-# NOINLINE enumFromTo #-}
enumFromTo :: Int -> Int -> [Int]
enumFromTo from@(I# m) to@(I# n) =
  if isTrue# (m ># n) then [] else
    from : enumFromTo (I# (m +# 1#)) to

test = case sumlen (enumFromTo 1 100) of
         (# sum_, len_ #) -> sum_ == 5050 && len_ == 100
