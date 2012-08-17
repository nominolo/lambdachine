{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.TraceCall where

import GHC.Bool
import GHC.Prim
import GHC.Types

{-# NOINLINE f #-}
f :: Int# -> Int# -> Int#
f x y = x +# y -# 3#

loop :: Int# -> Int# -> Int#
loop 0# acc = acc
loop n acc =
  let !acc' = f (n +# 5#) acc in
  loop (n -# 1#) acc'

test = loop 123# 1# ==# 7873#