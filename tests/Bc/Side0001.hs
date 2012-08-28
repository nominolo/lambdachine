{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Side0001 where

import GHC.Prim
import GHC.Types
import GHC.Base

--import Debug.Trace
--import Prelude

-- Test for side traces

-- Emulates Bresenham line drawing without actually drawing any pixels.
loop :: Int# -> Int# -> Int# -> Int# -> Int# -> Int
-- loop x  y d _ _ | trace (show (I# x, I# y, I# d)) False = undefined
loop 0# y d up down = I# y
loop x  y d up down =
  let !x' = x -# 1# in
  if d <=# 0# then loop x' y         (d +# up)   up down
              else loop x' (y +# 1#) (d -# down) up down

test = loop 500# 0# 1# 3# 4# == 214
