{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module Bc.RealWorld where

import GHC.Prim
import GHC.Types

{-# NOINLINE f #-}
f :: State# RealWorld -> Int
f _s = 42

test = case f realWorld# of
         I# n -> n ==# 42#
