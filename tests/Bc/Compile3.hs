{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bc.Compile3 where

import GHC.Prim
-- import GHC.Base

{-# NOINLINE f #-}
f :: Int# -> Int# -> Int#
f x y = g y x


{-# NOINLINE g #-}
g :: Int# -> Int# -> Int#
g x y = x -# y

test = f 4# ==# 1#
