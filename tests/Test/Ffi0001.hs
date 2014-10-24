{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc

module Test.Ffi0001 where

import GHC.Types ( Int(..) )

foreign import ccall unsafe "ctype.h isdigit" cIsDigit :: Int -> Int

f = cIsDigit 65
