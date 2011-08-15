{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
-- RUN: %bc
module Bc.Simpl0001 where

import GHC.Prim
import GHC.Bool

g :: Int# -> Int#
g y = y +# 42#
{-# NOINLINE g #-}

f :: Int# -> Int# -> Int#
f a b = let !x = a *# b in
      let !w = a +# a +# x in
      let !y = g w in
      let !z = y +# x in
      if y ># 0# then z else x +# (z *# b)