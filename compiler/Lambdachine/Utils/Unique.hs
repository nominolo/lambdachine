module Lambdachine.Utils.Unique (
  Unique,
  {-uniqueFromInt,-} intFromUnique, newUniqueSupply,
  bogusUnique,
  Supply, split, split2, split3, split4, supplyValue, modifySupply,
) where

import Lambdachine.Utils.Pretty

import Data.Array.Unboxed
import Data.Supply

newtype Unique = Unique Int
  deriving (Eq, Ord, Show)

instance Pretty Unique where
  ppr (Unique u) 
    | u /= 0 = text (toBaseXString u)
    | otherwise = empty

newUniqueSupply :: IO (Supply Unique)
newUniqueSupply = newSupply (Unique 1) (\(Unique n) -> Unique (n + 1))

bogusUnique :: Unique
bogusUnique = Unique 0

uniqueFromInt :: Int -> Unique
uniqueFromInt n = Unique n

intFromUnique :: Unique -> Int
intFromUnique (Unique n) = n

baseXDigits :: UArray Int Char
baseXDigits = listArray (0, n) digits
  where
    digits = ['0'..'9']++['a'..'z']++['A'..'Z']
    n = length digits
    -- TODO: filter out 0/O 1/l ?

baseX :: Int
baseX = snd (bounds baseXDigits) + 1

-- | Turn number into base-X string.
toBaseXString :: Int -> String
toBaseXString n | n < baseX = [baseXDigits ! n]
toBaseXString n =
  let (n', offs) = n `divMod` baseX in
  baseXDigits ! offs : toBaseXString n'
