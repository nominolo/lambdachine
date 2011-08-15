{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.I#!con_info 0x000000000000002a

-- Currently fails with
-- No instance for (GHC.Num.Num Int)
--   arising from the literal `42'
-- Possible fix: add an instance declaration for (GHC.Num.Num Int)
-- In the expression: 42
-- In an equation for `test': test = 42 
-- XFAIL: *
module Bc.Bc0006 where

import GHC.Prim
import GHC.Bool
import GHC.Integer (Integer(..))

data Int = I# Int#

fromInteger :: Integer -> Int
fromInteger = fromInteger

test :: Int
test = 42