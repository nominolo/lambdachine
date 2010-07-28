module Lambdachine.Utils.Unique (
  Unique,
  {-uniqueFromInt,-} intFromUnique, newUniqueSupply,
  bogusUnique,
  Supply, split, split2, split3, split4, supplyValue, modifySupply,
  mkBuiltinUnique, fromExternalUnique
) where

import Lambdachine.Utils.Pretty

import Data.Array.Unboxed
import Data.Bits ( shiftL, shiftR, (.|.), (.&.) )
import Data.Char ( ord, chr )
import Data.Supply

-- | A 'Unique' is simply an integer value.  It is used to quickly
-- compare two objects for identity.  A finite map indexed by objects
-- with an associated 'Unique' may also be implemented more
-- efficiently.
--
-- A unique consists of a 7 bit name space (usually written as a
-- 'Char') and a 24 bit number.  The name space (see first argument to
-- 'newUniqueSupply') allows us to have several independent unique
-- supplies.  (E.g., one for primitives, one for external symbols,
-- etc.)
--
-- 'Unique's are pretty-printed compactly by using a large base
-- representation (around base 64).  E.g., A 'Unique' with namespace
-- @a@ and number @54321@ is written as @axQN@
newtype Unique = Unique Int
  deriving (Eq, Ord)

instance Pretty Unique where
  ppr u0@(Unique u) 
    | u /= 0 = text (show u0)
    | otherwise = empty

instance Show Unique where
  show u0@(Unique u) =
    uniqueNameSpace u0 : toBaseXString (u .&. 0x00ffffff)

-- | Create a new 'Unique' supply.  The first argument encodes the
-- name space (as a character for easier printing).
--
-- It is up to the user to not create multiple supplies with the same
-- name space.
newUniqueSupply :: Char -> IO (Supply Unique)
newUniqueSupply c | ord c < 128 =
  newSupply u0 (\(Unique n) -> Unique (n + 1))
 where
   u0 = Unique ((ord c `shiftL` 24) .|. 1)
newUniqueSupply _ = error "newUniqueSupply: char must be 8 bits"

-- | Use only for defining 'Unique's for built-in types.  Uses name
-- space @%@.
mkBuiltinUnique :: Int -> Unique
mkBuiltinUnique = mk_unique '%'

-- | Turn a GHC Unique into our 'Unique'.
--
-- To ensure that no GHC Unique overlaps with locally generated
-- Uniques, we tag GHC-imported uniques by setting the highest bit
-- (the sign bit).  Assumes that GHC name spaces are at most 7 bits.
--
fromExternalUnique :: Int -> Unique
fromExternalUnique n
  | n .&. 0x80000000 == 0 = Unique (n .|. 0x80000000)
  | otherwise = error $ "fromGhcUnique: GHC Unique too large."

-- | Return the name space of the given 'Unique'.
uniqueNameSpace :: Unique -> Char
uniqueNameSpace (Unique u) =
  chr ((u `shiftR` 24) .&. 0x7f)

mk_unique :: Char -> Int -> Unique
mk_unique c n | ord c < 128 = Unique u
 where u = (ord c `shiftL` 24) .|. (n .&. 0x00ffffff)

bogusUnique :: Unique
bogusUnique = Unique 0

uniqueFromInt :: Int -> Unique
uniqueFromInt n = Unique n

intFromUnique :: Unique -> Int
intFromUnique (Unique n) = n

baseXDigits :: UArray Int Char
baseXDigits = listArray (0, n-1) digits
  where
    digits = ['A'..'Z']++['a'..'z']++['0'..'9']++['+','$']
    n = length digits
    -- TODO: filter out 0/O 1/l ?

baseX :: Int
baseX = snd (bounds baseXDigits) + 1

-- | Turn number into base-X string.
toBaseXString :: Int -> String
toBaseXString n | n < baseX = [baseXDigits ! n]
toBaseXString n =
  let (n', offs) = n `quotRem` baseX in
  baseXDigits ! offs : toBaseXString n'
