{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
module Bc.Basic0007 where

import GHC.Prim
import GHC.Types

test = raise# (42 :: Int)
