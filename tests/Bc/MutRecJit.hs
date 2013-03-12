{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}

module Bc.MutRecJit where

import GHC.Base
import GHC.Integer
import GHC.Num

mk :: Int# -> [[Int]] -> [[Int]]
mk 0# !acc = acc
mk n !acc =
  let ns = I# n : ns in
  case ns of
    ns' -> mk (n -# 1#) (ns' : acc)

use :: Int# -> [[Int]] -> Int
use acc [] = I# acc
use acc ((I# x:I# y:_):xys) =
  use (x +# y +# acc) xys
use acc (_:xys) = use acc xys

test = use 0# (mk 100# []) == 10100
