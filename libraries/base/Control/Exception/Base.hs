{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module Control.Exception.Base where

import GHC.Prim

irrefutPatError :: Addr# -> a
irrefutPatError = raise# patError

patError :: Addr# -> a
patError = raise# patError
