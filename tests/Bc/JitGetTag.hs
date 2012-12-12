{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bc.JitGetTag where

import GHC.Prim
import GHC.List
import GHC.Base

data ABC = A | B | C

sumtags :: [a] -> Int# -> Int#
sumtags [] acc = acc
sumtags (x:xs) acc =
  case x of x' -> case dataToTag# x' of tag -> sumtags xs (acc +# tag)

test = case sumtags (replicate (I# 100#) C) 0# of
         ts -> ts ==# 200#
--         ts -> I# ts
