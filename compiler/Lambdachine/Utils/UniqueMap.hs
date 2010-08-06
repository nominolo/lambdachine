-- | Efficient finite maps for keys with an associated 'Unique'.
--
-- Uses the 'Unique' of the key to implement the map in terms of
-- Patricia trees.
--
-- Currently only exports a minimum set of primitives.  Add more if
-- needed.
--
-- See also: "Data.IntMap".
module Lambdachine.Utils.UniqueMap 
  ( -- * Type
    UniqueMap,
    -- * Query
    lookupUM,
    -- * Construction
    emptyUM, singletonUM,
    -- * Insertion
    insertUM,
    -- * Deletion\/Update
    deleteUM,
    -- * Combine
    unionUM,
  )
where

import Lambdachine.Utils.Unique

import qualified Data.IntMap as IM
import Data.Monoid

-- | A map from 'Uniquable' keys to elements of any type.
newtype UniqueMap a = UniqueMap (IM.IntMap a)
  deriving (Eq, Ord)

instance Monoid (UniqueMap a) where
  mempty = emptyUM
  mappend = unionUM

instance Functor UniqueMap where
  fmap f (UniqueMap m) = UniqueMap (IM.map f m)

getKey :: Uniquable a => a -> Int
getKey a = intFromUnique (getUnique a)

emptyUM :: UniqueMap a
emptyUM = UniqueMap IM.empty

singletonUM :: Uniquable key => key -> a -> UniqueMap a
singletonUM key val =
  UniqueMap (IM.singleton (getKey key) val)

insertUM :: Uniquable key => key -> a -> UniqueMap a -> UniqueMap a
insertUM key val (UniqueMap m) =
  UniqueMap (IM.insert (getKey key) val m)

lookupUM :: Uniquable key => UniqueMap a -> key -> Maybe a
lookupUM (UniqueMap m) k = IM.lookup (getKey k) m

deleteUM :: Uniquable key => key -> UniqueMap a -> UniqueMap a
deleteUM k (UniqueMap m) = UniqueMap $ IM.delete (getKey k) m

-- | Left-biased union.  If both maps contain the same key, the new
-- value will be taken from the left map.
unionUM :: UniqueMap a -> UniqueMap a -> UniqueMap a
unionUM (UniqueMap m1) (UniqueMap m2) = UniqueMap (m1 `IM.union` m2)
