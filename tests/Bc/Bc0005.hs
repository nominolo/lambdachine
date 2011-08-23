{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.False`con_info
module Bc.Bc0005 where

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

infList :: Int -> List Int
infList n =
  let l1 = Cons n l2
      l2 = Cons n l1
  in l1

test :: Bool
test = null (infList (I# 1#))
