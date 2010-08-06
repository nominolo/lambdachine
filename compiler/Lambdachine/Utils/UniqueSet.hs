-- | Efficient sets for elements with an associated 'Unique'.
--
-- Operations over @UniqueSet a@ tend to be faster than a regular @Set
-- a@ by using the 'Unique' attached to each @a@.
--
module Lambdachine.Utils.UniqueSet where

import Lambdachine.Utils.Unique
import Lambdachine.Utils.UniqueMap
import Data.Maybe ( isJust )

newtype UniqueSet a = UniqueSet (UniqueMap a)

emptyUS :: UniqueSet a
emptyUS = UniqueSet emptyUM

singletonUS :: Uniquable a => a -> UniqueSet a
singletonUS x = UniqueSet (singletonUM x x)

lookupUS :: Uniquable a => UniqueSet a -> a -> Maybe a
lookupUS (UniqueSet m) a = lookupUM m a

elemUS :: Uniquable a => a -> UniqueSet a -> Bool
elemUS x us = isJust (lookupUS us x)

insertUS :: Uniquable a => a -> UniqueSet a -> UniqueSet a
insertUS a (UniqueSet m) = UniqueSet $ insertUM a a m