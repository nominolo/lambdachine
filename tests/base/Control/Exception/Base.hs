{-# LANGUAGE NoImplicitPrelude, MagicHash #-}

module Control.Exception.Base where

import GHC.Prim

irrefutPatError :: Addr# -> a
irrefutPatError = irrefutPatError

patError :: Addr# -> a
patError = patError
