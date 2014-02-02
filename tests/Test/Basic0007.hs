{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc
module Test.Basic0007 where

import GHC.Prim

data Int = I# Int#

test = raise# (I# 42#)
