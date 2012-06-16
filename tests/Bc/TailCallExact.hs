{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.TailCallExact where

import GHC.Bool
import GHC.Prim

--import GHC.Base

{-# NOINLINE f #-}
f :: Int# -> Int#
f n = g (n +# 3#) (n +# 4#)

{-# NOINLINE g #-}
g :: Int# -> Int# -> Int#
g x y = x *# y

test = f 5# ==# 72#
