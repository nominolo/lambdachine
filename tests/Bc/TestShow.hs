{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
module Bc.TestShow where

import GHC.Base
import GHC.Show
import GHC.Num

data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving (Eq, Show)

test = show (Branch (Leaf (-5)) (Branch (Leaf (6 :: Int)) (Leaf 7))) ==
         "Branch (Leaf (-5)) (Branch (Leaf 6) (Leaf 7))"
