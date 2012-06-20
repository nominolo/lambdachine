{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.TailCallPap where

import GHC.Bool
import GHC.Prim
import GHC.Types

--import GHC.Base

{-# NOINLINE f #-}
f :: Int# -> Int# -> Int#
f x y = x +# y

{-# NOINLINE g #-}
g :: Int# -> (Int# -> Int# -> Int#) -> [Int# -> Int#]
g a f' = [f' a]

test = case g 4# f of
         h:_ -> case h 5# of
                 9# -> True
                 _ -> False
         _ -> False
