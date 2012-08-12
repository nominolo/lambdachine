{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.SumMemLoad where

import GHC.Bool
import GHC.Prim
import GHC.Types

data X = X Int# Int# X

xs = X 1# 2# (X 3# 4# xs)

loop :: Int# -> Int# -> X -> Int#
loop acc s (X inc1 dec1 x') =
  if s <=# 0# then acc else
    loop (acc +# inc1) (s -# dec1) x'

test = (loop 0# 1000# xs) ==# 668#
