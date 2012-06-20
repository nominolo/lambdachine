{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Gc01 where

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

test = length (f 4000# []) == 4000
