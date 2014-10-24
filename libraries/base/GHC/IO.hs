{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , RankNTypes
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.IO
-- Copyright   :  (c) The University of Glasgow 1994-2002
-- License     :  see libraries/base/LICENSE
-- 
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC Extensions)
--
-- Definitions for the 'IO' monad and its friends.
--
-----------------------------------------------------------------------------

module GHC.IO (
        IO(..), unIO, failIO, liftIO,
        unsafePerformIO, unsafeInterleaveIO,
        unsafeDupablePerformIO, unsafeDupableInterleaveIO,
        noDuplicate,

        -- To and from from ST
        stToIO, ioToST, unsafeIOToST, unsafeSTToIO,

        FilePath,

        catchException, catchAny, throwIO,
        mask, mask_, uninterruptibleMask, uninterruptibleMask_, 
        MaskingState(..), getMaskingState,
        unsafeUnmask,
        onException, bracket, finally, evaluate
    ) where

import GHC.Base
import GHC.ST
import GHC.Exception
import GHC.Show
import Data.Maybe

import {-# SOURCE #-} GHC.IO.Exception ( userError )
