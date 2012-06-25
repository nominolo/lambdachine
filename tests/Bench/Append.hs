{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
{-# OPTIONS_GHC -O0 #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info 
module Bench.Append where

import GHC.Types
import GHC.Integer
import GHC.Num
import GHC.Base

root xs ys zs = (xs ++ ys) ++ zs

{- # NOINLINE replicate #-}
replicate :: Int -> Int -> [Int]
replicate n x =
  if n == 0 then [] else x : replicate (n - 1) x

{- # NOINLINE length #-}
length :: [a] -> Int
length l = len l 0#
  where
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)

{- # NOINLINE test1 #-}
test1 n =
  length (root (replicate n 1) (replicate n 2) (replicate n 3))
    == (n + n + n)

test = test1 200 -- max: 1820
