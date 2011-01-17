{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.Integer (
  Integer,
  smallInteger
) where

import GHC.Prim
import GHC.Integer.Type

{-# INLINE smallInteger #-}
smallInteger :: Int# -> Integer
smallInteger i = S# i
