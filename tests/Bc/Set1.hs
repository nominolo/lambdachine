{-# LANGUAGE NoImplicitPrelude #-}
module Bc.Set1 where

import Data.Set as Set

import GHC.Base

test =
  (Set.fromList [(1::Int)..5] `Set.union` Set.fromList [10..14]) ==
    (Set.fromList ([1..5] ++ [10..14]))
