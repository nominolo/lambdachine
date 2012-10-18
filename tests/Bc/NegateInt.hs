{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
module Bc.NegateInt where

import GHC.Base
import GHC.Prim
import GHC.Num

loop :: Int# -> Int# -> Int
loop 0# x = I# x
loop n  x = loop (n -# 1#) (negateInt# x)

test = loop 51# 4# == -4
