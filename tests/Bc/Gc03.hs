{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Gc03 where

import GHC.Bool
import GHC.Prim
import GHC.List
import GHC.Types
import GHC.Base

-- Tests GCing of PAPs.

{-# NOINLINE f #-}
f :: Int# -> [Box] -> [Box]
f 0# acc = acc
f n acc =
  let !val = boxit n silly in
  let !acc' = val : acc in
  f (n -# 1#) acc'

data Box = Box !(Int -> Int#)

-- Creates a PAP
{-# NOINLINE boxit #-}
boxit :: Int# -> (Int -> Int# -> Int -> Int#) -> Box
boxit n f = Box (f (I# n) n)

silly :: Int -> Int# -> Int -> Int#
silly (I# z) x (I# y) = z +# x *# y

check :: Int# -> Int# -> [Box] -> Bool
check len n [] = n ==# (len +# 1#)
check len n (Box f:xs) =
  if f (I# 1#) ==# (n +# n) then check len (n +# 1#) xs else False

test =
  let !n = 3000# in check n 1# (f n [])
