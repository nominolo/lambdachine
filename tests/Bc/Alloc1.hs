{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Alloc1 where

import GHC.Prim
import GHC.List
import GHC.Types
import GHC.Base

{-# NOINLINE f #-}
f :: Int# -> [Int] -> [Int]
f 0# acc = acc
f n acc = let !acc' = I# n : acc in
          f (n -# 1#) acc'

check :: Int# -> Int# -> [Int] -> Bool
check len n [] = isTrue# (n ==# (len +# 1#))
check len n (I# m:xs) =
  if isTrue# (n ==# m) then check len (n +# 1#) xs else False

test =
  let !n = 100# in check n 1# (f n [])
