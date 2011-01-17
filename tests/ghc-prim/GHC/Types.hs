{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.Types where
-- (Char(..), Int(..), Float(..), Double(..), IO(..)) where

import GHC.Prim

data Int = I# Int#

infixr 5 :
data [] a = [] | a : [a]
