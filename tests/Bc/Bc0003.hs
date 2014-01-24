{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc
module Bc.Bc0003 where

import GHC.Prim
import GHC.Types

-- data Int = I# Int#

data List a = Nil | Cons a (List a)

mapPlusOne :: List Int -> List Int
mapPlusOne l = go l
 where go Nil = Nil
       go (Cons x xs) = 
         let !y = plusOne x in 
         Cons y (go xs)

plusOne :: Int -> Int
plusOne (I# x) = I# (x +# 1#)

gtInt :: Int -> Int -> Bool
gtInt (I# m) (I# n) = m ># n

plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)

upto :: Int -> Int -> List Int
upto from !to =
  if from `gtInt` to then
    Nil
   else
    Cons from (upto (plusOne from) to)

forceSpine :: List a -> List a
forceSpine Nil = Nil
forceSpine (Cons x xs) = xs `seq` Cons x xs

test = forceSpine (mapPlusOne (upto (I# 0#) (I# 10#)))
