{-# OPTIONS_GHC -XNoImplicitPrelude #-}
module Text.Read.Lex
  -- lexing types
  ( Lexeme(..)  -- :: *; Show, Eq
{-
  -- lexer      
  , lex         -- :: ReadP Lexeme      Skips leading spaces
  , hsLex       -- :: ReadP String
  , lexChar     -- :: ReadP Char        Reads just one char, with H98 escapes

  , readIntP    -- :: Num a => a -> (Char -> Bool) -> (Char -> Int) -> ReadP a
  , readOctP    -- :: Num a => ReadP a 
  , readDecP    -- :: Num a => ReadP a
  , readHexP    -- :: Num a => ReadP a
-}
  )
 where

import Text.ParserCombinators.ReadP

import GHC.Base
import GHC.Num( Num(..), Integer )
import GHC.Show( Show(..) )
import GHC.Real( Ratio(..), Integral, Rational, (%), fromIntegral, 
                 toInteger, (^), (^^), infinity, notANumber )
import GHC.List
import GHC.Enum( maxBound )
import Data.Maybe
import Control.Monad

-- ^ Haskell lexemes.
data Lexeme
  = Char   Char         -- ^ Character literal
  | String String       -- ^ String literal, with escapes interpreted
  | Punc   String       -- ^ Punctuation or reserved symbol, e.g. @(@, @::@
  | Ident  String       -- ^ Haskell identifier, e.g. @foo@, @Baz@
  | Symbol String       -- ^ Haskell symbol, e.g. @>>@, @:%@
  | Int Integer         -- ^ Integer literal
  | Rat Rational        -- ^ Floating point literal
  | EOF
 deriving (Eq, Show)
