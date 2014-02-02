{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> Test.Basic0005.I#`con_info 1
{-# OPTIONS_GHC -Wall #-}
module Test.Basic0005 where

import GHC.Prim

data Int = I# Int#

data List a = Nil | Cons a (List a)

{-# NOINLINE l1 #-}
l1 :: Int -> List Int
l1 n = Cons n (Cons n (Cons n Nil))

sum :: List Int -> Int
sum ls = go 0# ls
 where
   go acc Nil = I# acc
   go acc (Cons (I# n) xs) = go (acc +# n) xs

test = case sum (l1 (I# 3#)) of
         I# n -> I# (n ==# 9#)


