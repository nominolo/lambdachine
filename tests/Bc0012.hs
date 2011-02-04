{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
module Bc0012 where

import GHC.Base
import GHC.Integer
import GHC.Num

data Tree a = Leaf a | Branch (Tree a) (Tree a)

class MyEq a where
  equal :: a -> a -> Bool
  not_equal :: a -> a -> Bool
  not_equal x y = not (equal x y)

instance MyEq Bool where
  equal True True = True
  equal False False = True
  equal _ _ = False
