{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc
module Bc.Bc0008 where

import GHC.Prim

f :: Float# -> Float#
f x = (3.14#)

g :: Char# -> Char#
g x = 'a'#

h :: Word# -> Word#
h w = 42##

i :: Int# -> Addr#
i _ = "hello"#
