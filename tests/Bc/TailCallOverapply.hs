{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.TailCallOverapply where

import GHC.Prim
import GHC.Types

--import GHC.Base

class MyNum a where
  mul :: a -> a -> a

instance MyNum Int where
  mul (I# x) (I# y) = I# (x *# y)

{-# NOINLINE f #-}
f :: Int# -> Int
f n = g (I# (n +# 3#)) (I# (n +# 4#))

{-# NOINLINE g #-}
g :: MyNum a => a -> a -> a
g x y = mul x y

test = case f 5# of
         I# 72# -> True
         _ -> False
