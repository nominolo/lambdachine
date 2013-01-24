{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}
module Bc.Compile1 where

import GHC.Base
import Data.Maybe
import GHC.Num

{-# NOINLINE f #-}
f :: (a -> b) -> a -> b
f k x = k x

{-# NOINLINE just #-}
just :: a -> Maybe a
just = Just

{-# NOINLINE g #-}
g :: Int -> Maybe Int -> Maybe Int
g n mb =
  case mb of
    Nothing ->
      Just 0
    Just x ->
      let {-# NOINLINE h #-}
          h f1 y = f1 (x + y) in
      let !mm = h Just 3 in
      case mm of
        Just i -> Just (i + x)

{-# NOINLINE test #-}
test = case g 1 (Just 10) of
         Nothing -> True
         Just k -> k == 23
