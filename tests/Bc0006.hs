{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
module Bc0006 where

import GHC.Prim
import GHC.Bool
import GHC.Integer (Integer(..))

data Int = I# Int#

fromInteger :: Integer -> Int
fromInteger = fromInteger

test :: Int
test = 42