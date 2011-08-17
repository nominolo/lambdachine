{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- RUN: %bc
module Bc0004 where

import GHC.Prim

data Int = I# Int#

data List a = Nil | Cons a (List a)

