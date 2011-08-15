{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Bc0007 where

import GHC.Base
import GHC.Prim
--import GHC.Bool
--import GHC.Types

len :: [a] -> Int
len [] = I# 0#
len (x:xs) = I# 1#

test = I# 4# == I# 3#
