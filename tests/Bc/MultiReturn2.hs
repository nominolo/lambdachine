{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}
module Bc.MultiReturn2 where

import GHC.Prim
import GHC.Types
import GHC.Num
import GHC.Base

{-# NOINLINE g #-}
g :: Int# -> State# RealWorld -> (# State# RealWorld, Int #)
g n s = (# s, I# (n +# 1#) #)

test = case g 5# realWorld# of
         (# s', I# m #) -> isTrue# (m ==# 6#)
