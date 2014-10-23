{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Bc.NopPrims where

import GHC.Prim
import GHC.Types( Bool(..) )

test = case ord# (chr# 42#) of
         42# ->
           case word2Int# (int2Word# 45#) of
             45# -> True
             _ -> False
         _ -> False
