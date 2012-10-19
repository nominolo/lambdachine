{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
module Bc.BitOps where

import GHC.Prim
import GHC.Base

loop :: Int# -> Int# -> Int
loop 0# acc = I# acc
loop n acc =
  let !w0 = not# (int2Word# n)
      !w1 = w0 `and#` int2Word# 823719#
      !w2 = w1 `xor#` int2Word# 90234342#
      !w3 = w2 `or#` int2Word# 0x8001#
      !n' = word2Int# w3
  in loop (n -# 1#) (acc +# n')

test = case loop 101# 0# of
         res@(I# res') -> res' ==# 9192643473#