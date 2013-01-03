{-# LANGUAGE NoImplicitPrelude #-}
module Bc.Map1 where

import Data.Map as Map
import Data.List ( zip )
import GHC.Base

test = (m1 `Map.union` m2) == m3
 where
   m1 = Map.fromList (zip [(1::Int)..5] ['a'..])
   m2 = Map.fromList (zip [10..15] ['m'..])
   m3 = Map.fromList (zip [(1::Int)..5] ['a'..] ++ zip [10..15] ['m'..])

