{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.Integer (
  Integer,
  smallInteger, toInt#
) where

import GHC.Prim
import GHC.Types
import GHC.Integer.Type

{-# INLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = S# i

toInt# :: Integer -> Int#
{-# RULES "toInt#" forall i. toInt# (S# i) = i #-}
toInt# (S# i)   = i
toInt# (J# s d) = 0#  -- XXX: for now
