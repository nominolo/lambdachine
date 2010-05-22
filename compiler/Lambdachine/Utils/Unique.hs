module Lambdachine.Utils.Unique (
  Unique,
  {-uniqueFromInt,-} intFromUnique, newUniqueSupply,
  bogusUnique,
  Supply, split, split2, split3, split4, supplyValue, modifySupply,
  mkBuiltinUnique
) where

import Lambdachine.Utils.Pretty

import Data.Array.Unboxed
import Data.Bits ( shiftL, (.|.), (.&.) )
import Data.Char ( ord )
import Data.Supply

newtype Unique = Unique Int
  deriving (Eq, Ord, Show)

instance Pretty Unique where
  ppr (Unique u) 
    | u /= 0 = text (toBaseXString (u .&. 0x00ffffff))
    | otherwise = empty

newUniqueSupply :: Char -> IO (Supply Unique)
newUniqueSupply c | ord c < 256 =
  newSupply u0 (\(Unique n) -> Unique (n + 1))
 where
   u0 = Unique ((ord c `shiftL` 24) .|. 1)
newUniqueSupply _ = error "newUniqueSupply: char must be 8 bits"

mkBuiltinUnique :: Int -> Unique
mkBuiltinUnique = mk_unique 'p'

mk_unique :: Char -> Int -> Unique
mk_unique c n | ord c < 256 = Unique u
 where u = (ord c `shiftL` 24) .|. (n .&. 0x00ffffff)

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
