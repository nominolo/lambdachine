{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bench.Append where

import GHC.Types
import GHC.Integer
import GHC.Num
import GHC.Base

root xs ys zs = (xs ++ ys) ++ zs

replicate :: Int -> Int -> [Int]
replicate n x =
  if n == 0 then [] else x : replicate (n - 1) x

length :: [a] -> Int
length [] = 0
length (x:xs) = 1 + length xs

test =
  let n = 25 in
  length (root (replicate n 1) (replicate n 2) (replicate n 3))
    == (n + n + n)