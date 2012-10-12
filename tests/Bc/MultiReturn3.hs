{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}
module Bc.MultiReturn3 where

import GHC.Prim
import GHC.Types
import GHC.Num
import GHC.Base

{-# NOINLINE g #-}
g :: Int# -> State# RealWorld -> (# Int, State# RealWorld, Int #)
g n s = (# I# (n +# 5#), s, I# (n +# 1#) #)

test = case g 5# realWorld# of
         (# I# x, s', I# y #) -> x ==# 10# && y ==# 6#
