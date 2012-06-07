{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Bc0014 where

import GHC.Prim
import GHC.Bool

data D = A | B | C

{-# NOINLINE f #-}
f :: D -> Int#
f d = case d of
        C -> 3#
        _ -> 4#

test = case f A of
         4# -> case f C of
                 3# -> True
                 _ -> False
         _ -> False
