module Lambdachine.Name where

import Lambdachine.Utils

data Name = Name {-# UNPACK #-} !Unique String
  deriving (Eq, Ord)

name :: String -> Name
name n = Name bogusUnique n

freshName :: Supply Unique -> String -> Name
freshName u n = Name (supplyValue u) n

-- | For internal use only.
mkBuiltinName :: Unique -> String -> Name
mkBuiltinName u n = Name u n

instance Show Name where
  show (Name u n) = n

instance Pretty Name where
  ppr (Name u n)
   | u == bogusUnique = text n
   | otherwise = text n -- <> pale (char '_' <> ppr u)

