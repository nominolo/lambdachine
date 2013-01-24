{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
module Bc.Compile2 where

import GHC.Base

{-# NOINLINE f #-}
f :: Int -> Int -> Int
f _ x = x

{-# NOINLINE g #-}
g :: Int -> Int -> Int
g _ _ = 0

test = f 4 5 == 5
