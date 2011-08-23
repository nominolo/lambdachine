{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.I#`con_info 3
module Bc.Bc0013 where

import GHC.Prim
import GHC.Bool
import GHC.Classes
import GHC.Types

f :: Int# -> Int# -> Int#
f i j =
  if i ># 0# && j ># 0# then 8# `remInt#` 5#
                        else 16#

test = I# (f 3# 4#)
