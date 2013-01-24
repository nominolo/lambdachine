{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Bc.PointerInfo where

import GHC.Prim

data Digits = Some !Digit !Digits
            | None
type Digit = Word#
