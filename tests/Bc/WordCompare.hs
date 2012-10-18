{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
module Bc.WordCompare where

import GHC.Prim
import GHC.Base

loop :: Word# -> Int# -> Int#
loop n x = 
  if n `leWord#` int2Word# 5#
--  if word2Int# n <=# 5#
   then x
   else loop (int2Word# (word2Int# n +# 1#))
             (x +# 1#)

run :: Int# -> Bool
run n = loop (int2Word# (0# -# n)) 0# ==# n

test = run 10#
