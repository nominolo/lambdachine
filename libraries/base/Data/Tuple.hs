{-# OPTIONS_GHC -XNoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- |
-- Module      :  Data.Tuple
-- Copyright   :  (c) The University of Glasgow 2001
-- License     :  BSD-style (see the file libraries/base/LICENSE)
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- The tuple data types, and associated functions.
--
-----------------------------------------------------------------------------

module Data.Tuple
  ( fst         -- :: (a,b) -> a
  , snd         -- :: (a,b) -> a
  , curry       -- :: ((a, b) -> c) -> a -> b -> c
  , uncurry     -- :: (a -> b -> c) -> ((a, b) -> c)
  , swap        -- :: (a,b) -> (b,a)
  )
where

import GHC.Base
-- We need to depend on GHC.Base so that
-- a) so that we get GHC.Bool, GHC.Classes, GHC.Ordering

-- b) so that GHC.Base.inline is available, which is used
--    when expanding instance declarations

import GHC.Tuple
-- We must import GHC.Tuple, to ensure sure that the 
-- data constructors of `(,)' are in scope when we do
-- the standalone deriving instance for Eq (a,b) etc

-- import GHC.Unit ()

default ()              -- Double isn't available yet


-- | Extract the first component of a pair.
fst                     :: (a,b) -> a
fst (x,_)               =  x

-- | Extract the second component of a pair.
snd                     :: (a,b) -> b
snd (_,y)               =  y

-- | 'curry' converts an uncurried function to a curried function.
curry                   :: ((a, b) -> c) -> a -> b -> c
curry f x y             =  f (x, y)

-- | 'uncurry' converts a curried function to a function on pairs.
uncurry                 :: (a -> b -> c) -> ((a, b) -> c)
uncurry f p             =  f (fst p) (snd p)

-- | Swap the components of a pair.
swap                    :: (a,b) -> (b,a)
swap (a,b)              = (b,a)
