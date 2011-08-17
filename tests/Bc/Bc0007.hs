{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.False!con_info
module Bc.Bc0007 where

import GHC.Base
import GHC.Prim
--import GHC.Bool
--import GHC.Types

len :: [a] -> Int
len [] = I# 0#
len (x:xs) = I# 1#

test = I# 4# == I# 3#
