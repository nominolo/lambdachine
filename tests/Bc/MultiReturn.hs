{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}
module Bc.MultiReturn where

import GHC.Prim
import GHC.Types
import GHC.Num
import GHC.Base

{-# NOINLINE f #-}
f :: Int -> Int -> (# Int, Int #)
f x y =
  let !x' = x + 1
      y' = y + 3
  in (# x', y' #)

data A a = A a

-- This only compiles on GHC 7.6+ (and perhapas 7.4).  Max added some magic
-- transformation that auto-converts the types:
--
--     (# A1, ..., AN #) -> X   ~~>   A1 -> ... -> AN -> X
--
-- Unfortunately, this conversion seems to occur after CorePrep, so we
-- have to reproduce it in the bytecode compiler to support newer
-- versions of GHC.  Simple solution: don't support any GHC > 7.0.4

{-
g :: Int -> Int
g n = let y = f n n in 42

h :: Bool -> (# Int, Int #) -> (# Int, Int #)
h b n =
  let y = n in
  if b then (# 2, 1 #)
       else n
-}

-- This is a kind mismatch:
{-
k :: a -> a
k n = n

k1 = k (# 2, 3 #)

k2 :: (# Int, Int #) -> Int
k2 n = let a = A n in 42
-}

test = case f 4 5 of
         (# (I# n), (I# m) #) ->
           if isTrue# (n ==# 5#) then isTrue# (m ==# 8#) else False
