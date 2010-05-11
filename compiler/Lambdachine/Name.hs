module Lambdachine.Name where

import Lambdachine.Utils

data Name = Name Unique String
  deriving (Eq, Ord, Show)

name :: String -> Name
name n = Name bogusUnique n

instance Pretty Name where
  ppr (Name u n)
   | u == bogusUnique = text n
   | otherwise = text n <> char '_' <> ppr u

