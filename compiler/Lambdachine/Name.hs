module Lambdachine.Name where

import Lambdachine.Utils

data Name = Name Unique String
  deriving (Eq, Ord, Show)

name :: String -> Name
name n = Name bogusUnique n

freshName :: Supply Unique -> String -> Name
freshName u n = Name (supplyValue u) n

instance Pretty Name where
  ppr (Name u n)
   | u == bogusUnique = text n
   | otherwise = text n <> pale (char '_' <> ppr u)

