{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Gc02 where

import GHC.Bool
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
check len n [] = n ==# (len +# 1#)
check len n (I# m:xs) =
  if n ==# m then check len (n +# 1#) xs else False

-- Test that CAFs are properly followed by the GC.
--
-- 1. Evaluate a large CAF
-- 2. Allocate more data (triggers GC)
-- 3. Check CAF again.

theCAF = f 2000# []

test =
  case length theCAF of
    I# n ->
      if n /=# 2000# then False else
        let !n = 2000# in
        case check n 1# (f n []) of
          False -> False
          True ->
            check 2000# 1# theCAF
