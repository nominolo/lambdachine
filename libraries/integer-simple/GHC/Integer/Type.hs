{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module GHC.Integer.Type (
    Integer(..),
    Positive, Positives,
    Digits(..), Digit,
    List(..)
 ) where

import GHC.Prim
import GHC.Types ()

data Integer = Positive !Positive | Negative !Positive | Naught

-------------------------------------------------------------------
-- The hard work is done on positive numbers

-- Least significant bit is first

-- Positive's have the property that they contain at least one Bit,
-- and their last Bit is One.
type Positive = Digits
type Positives = List Positive

data Digits = Some !Digit !Digits
            | None
type Digit = Word#

-- XXX Could move [] above us
data List a = Nil | Cons a (List a)
