{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.SumNoAlloc where

import GHC.Bool
import GHC.Prim
import GHC.Types

loop :: Int# -> Int# -> Int#
loop acc s =
  if s <=# 0# then acc else
    loop (acc +# s) (s -# 1#)

test = loop 0# 1000# ==# 500500#
