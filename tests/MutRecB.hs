{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module MutRecB where

import GHC.Bool

import MutRecA

data B = B Bool A
