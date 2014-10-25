{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Paps where

import GHC.Prim
import GHC.Types

--import GHC.Base

{-# NOINLINE f #-}
f :: Int# -> Int -> Int# -> Int -> Int -> Int#
f a (I# b) c (I# d) (I# e)= a +# b *# c -# d *# e

data Box a = Box a

{-# NOINLINE g #-}
g :: Int# -> Int -> (Int# -> Int -> Int# -> Int -> Int -> Int#)
  -> Box (Int# -> Int -> Int -> Int#)
g a b f' = Box (f' a b)

test = case g 4# (I# 5#) f of
         Box h -> case h 6# 2 3 of n -> isTrue# (n ==# 28#)
