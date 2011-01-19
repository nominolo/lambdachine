{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module PreludeTests where

import GHC.Base
import GHC.Num

t1 :: Int -> Int -> Bool
t1 x y = x /= y

test1 = let !b = t1 3 4 in
  if b then t1 (I# 5#) (I# 5#) else False

test :: Int
test = 3 - 11 + 6 * 8 - (-2)

rep :: Int# -> [Int]
rep n = if n ==# 0# then [] else I# n : rep (n -# 1#)

--test = rep 3#
