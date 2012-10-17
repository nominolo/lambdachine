{-# LANGUAGE MagicHash, NoImplicitPrelude, UnboxedTuples, BangPatterns #-}
module Bc.Monoid where

import GHC.Base
import Data.Monoid
import GHC.List
import GHC.Enum

test = length ([(1::Int) .. 5] `mappend` [6 .. 10]) == 10
