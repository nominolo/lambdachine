{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
module Bc.Basic0001 where

import GHC.Prim

-- Define a datatype with a payload

data X = X Int#

