{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc
module Test.Basic0002 where

import GHC.Prim

-- Define a datatype with a payload

data X = X Int#
