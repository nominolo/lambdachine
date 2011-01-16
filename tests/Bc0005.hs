{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Bc0005 where

import GHC.Prim
import GHC.Bool

data Int = I# Int#

data List a = Nil | Cons a (List a)

null :: List a -> Bool
null Nil = True
null _ = False

plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)

gtInt :: Int -> Int -> Bool
gtInt (I# m) (I# n) = m ># n
