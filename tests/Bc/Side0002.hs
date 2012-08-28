{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Side0002 where

import GHC.Prim
import GHC.Types
import GHC.Base

loop :: Int# -> Int# -> Int# -> Int#
loop a b c =
  let !a' = a +# c in
  let !b' = if b ># 0# then b -# a' else b +# a' in
  if c ># 0# then loop a' b' (c -# 1#) else (a' -# b')

test = case loop 7345# 123# 23# of
         n -> n ==# 7366#