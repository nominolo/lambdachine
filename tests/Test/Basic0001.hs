{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc

module Test.Basic0001 where

import GHC.Prim

-- A simple top-level closure.
data X = X
