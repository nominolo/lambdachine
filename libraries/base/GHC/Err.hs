{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Err
       (
         absentErr                 -- :: a
       , divZeroError              -- :: a
       , overflowError             -- :: a

       , error                     -- :: String -> a

       , undefined                 -- :: a
       ) where

import GHC.Types
-- import GHC.Exception


{-# NOINLINE error #-}
error :: [Char] -> a
error s = error s -- throw (ErrorCall s)

undefined :: a
undefined = undefined -- error "Prelude.undefined"

absentErr :: a
absentErr = absentErr -- error "Oops! The program has entered an `absent' argument!\n"

{-# NOINLINE divZeroError #-}
divZeroError :: a
divZeroError = divZeroError -- throw DivideByZero

{-# NOINLINE overflowError #-}
overflowError :: a
overflowError = overflowError -- throw Overflow
