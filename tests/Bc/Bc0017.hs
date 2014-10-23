{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Bc0017 where

import GHC.Prim
import GHC.Types (isTrue#)

data Int = CInt Int#

data Box a = Box a

{-# NOINLINE f #-}
f :: Int# -> Box Int
f x = Box (CInt x)

test =
  case f 5# of
    Box (CInt n) -> isTrue# (n ==# 5#)

