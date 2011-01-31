{-# LANGUAGE TypeFamilies #-}
module Lambdachine.Utils.Unique (
  Unique, Uniquable(..),
  {-uniqueFromInt,-} intFromUnique, newUniqueSupply,
  bogusUnique,
  Supply, split, split2, split3, split4, supplyValue, modifySupply,
  mkBuiltinUnique, fromExternalUnique, unsafeMkUnique,
  unsafeMkUniqueNS,

  UniqueMap,
  emptyUM, unionUM, singletonUM,
  lookupUM, insertUM, insertWithUM, adjustUM,
  deleteUM, elementsUM, nullUM, sizeUM, fromListUM,
  adjustOrInsertUM, mapUM,

  UniqueSet,
  emptyUS, singletonUS, unionUS, foldUS, insertUS, deleteUS,
  nullUS, sizeUS, memberUS, fromListUS,
  differenceUS, intersectionUS,

) where

import Prelude hiding ( foldr )

import Lambdachine.Utils.Pretty hiding ( empty )

--import Lambdachine.Utils.Classes

import Data.Array.Unboxed
import Data.Bits ( shiftL, shiftR, (.|.), (.&.) )
import Data.Char ( ord, chr )
import Data.Supply
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Foldable ( Foldable(foldr), toList )

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
    | otherwise = mempty

instance Show Unique where
  show u0@(Unique u) =
    uniqueNameSpace u0 : reverse (toBaseXString (u .&. 0x00ffffff))

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
newUniqueSupply _ = error "newUniqueSupply: char must be 7 bits"

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

unsafeMkUnique :: Int -> Unique
unsafeMkUnique n = Unique n

unsafeMkUniqueNS :: Char -> Int -> Unique
unsafeMkUniqueNS = mk_unique

intFromUnique :: Unique -> Int
intFromUnique (Unique n) = n
{-# INLINE intFromUnique #-}

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

-- | Class for objects that have an associated unique id.
class Uniquable a where
  getUnique :: a -> Unique

instance Uniquable Int where
  getUnique n = unsafeMkUnique n

instance Uniquable Unique where
  getUnique u = u


-- -------------------------------------------------------------------

-- | A map from 'Uniquable' keys to values.
--
-- The 'key' type parameter is a phantom type variable.  It is
-- required for the 'MapLike' instance, but it also helps to avoid
-- accidentally inserting elements of the wrong key type into the map.
newtype UniqueMap key val = UM { unUM :: IM.IntMap val }

instance Pretty a => Pretty (UniqueMap k a) where
  ppr (UM m) =
    braces $ fillSep $ commaSep $ map ppr (IM.toList m)

instance Show a => Show (UniqueMap k a) where
  show (UM m) = "fromList " ++
    show [ (Unique k, v) | (k, v) <- IM.toList m ]

instance Monoid (UniqueMap k a) where
  mempty = emptyUM
  mappend = unionUM

uInt :: Uniquable a => a -> Int
uInt a = intFromUnique (getUnique a)

emptyUM :: UniqueMap k v
emptyUM = UM IM.empty

singletonUM :: Uniquable k => k -> v -> UniqueMap k v
singletonUM k v = UM (IM.singleton (uInt k) v)

nullUM :: UniqueMap k v -> Bool
nullUM (UM m) = IM.null m

sizeUM :: UniqueMap k v -> Int
sizeUM (UM m) = IM.size m

unionUM :: UniqueMap k v -> UniqueMap k v -> UniqueMap k v
unionUM (UM m1) (UM m2) = UM (IM.union m1 m2)

lookupUM :: Uniquable k => k -> UniqueMap k v -> Maybe v
lookupUM k (UM m) = IM.lookup (uInt k) m

insertUM :: Uniquable k => k -> v -> UniqueMap k v -> UniqueMap k v
insertUM k v (UM m) = UM (IM.insert (uInt k) v m)

insertWithUM :: Uniquable k => (v -> v -> v) -> k -> v
             -> UniqueMap k v -> UniqueMap k v
insertWithUM f k v (UM m) = UM (IM.insertWith f (uInt k) v m)

-- | Insert or modify a given value.
--
-- If a value is present, apply the modification function to it.  If
-- no value is present insert the given value.
adjustOrInsertUM :: Uniquable k =>
                       (v -> v)  -- ^ Modification function
                    -> k
                    -> v -- ^ Insert this value if none present.
                    -> UniqueMap k v -> UniqueMap k v
adjustOrInsertUM f k v (UM m) =
  UM (IM.insertWith (\_new old -> f old) (uInt k) v m)

deleteUM :: Uniquable k => k -> UniqueMap k v -> UniqueMap k v
deleteUM k (UM m) = UM (IM.delete (uInt k) m)

adjustUM :: Uniquable k => (v -> v) -> k
         -> UniqueMap k v -> UniqueMap k v
adjustUM f k (UM m) = UM (IM.adjust f (uInt k) m)

elementsUM :: UniqueMap k v -> [v]
elementsUM (UM m) = IM.elems m

fromListUM :: Uniquable k => [(k, v)] -> UniqueMap k v
fromListUM kvs = UM $ IM.fromList [ (uInt k, v) | (k, v) <- kvs ]

mapUM :: (v -> w) -> UniqueMap k v -> UniqueMap k w
mapUM f (UM m) = UM (IM.map f m)

newtype UniqueSet a = US { unUS :: IM.IntMap a }

instance Monoid (UniqueSet a) where
  mempty = emptyUS
  mappend = unionUS

instance Show a => Show (UniqueSet a) where
  show (US m) = "fromList " ++ show (toList m)

instance Pretty a => Pretty (UniqueSet a) where
  ppr (US m) = braces $ fillSep $ commaSep $ map ppr (toList m)

nullUS :: UniqueSet a -> Bool
nullUS (US m) = IM.null m

-- O(n)
sizeUS :: UniqueSet a -> Int
sizeUS (US m) = IM.size m

emptyUS :: UniqueSet a
emptyUS = US IM.empty

singletonUS :: Uniquable a => a -> UniqueSet a
singletonUS a = US (IM.singleton (uInt a) a)
{-# INLINE singletonUS #-}

memberUS :: Uniquable a => a -> UniqueSet a -> Bool
memberUS a (US m) = IM.member (uInt a) m

unionUS :: UniqueSet a -> UniqueSet a -> UniqueSet a
unionUS (US m1) (US m2) = US (IM.union m1 m2)

insertUS :: Uniquable a => a -> UniqueSet a -> UniqueSet a
insertUS a (US m) = US (IM.insert (uInt a) a m)

deleteUS :: Uniquable a => a -> UniqueSet a -> UniqueSet a
deleteUS a (US m) = US (IM.delete (uInt a) m)

foldUS :: (a -> b -> b) -> b -> UniqueSet a -> b
foldUS f z (US m) = IM.fold f z m

fromListUS :: Uniquable a => [a] -> UniqueSet a
fromListUS as = US $ IM.fromList [ (uInt a, a) | a <- as ]

differenceUS :: UniqueSet a -> UniqueSet a -> UniqueSet a
differenceUS (US m1) (US m2) = US (m1 `IM.difference` m2)

intersectionUS :: UniqueSet a -> UniqueSet a -> UniqueSet a
intersectionUS (US m1) (US m2) = US (m1 `IM.intersection` m2)

instance Foldable UniqueSet where
  foldr = foldUS
