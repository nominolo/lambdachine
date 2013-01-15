{-# LANGUAGE NoImplicitPrelude #-}
module Text.Read (
   -- * The 'Read' class
   Read(..),            -- The Read class
   ReadS,               -- String -> Maybe (a,String)

   reads,               -- :: (Read a) => ReadS a
   read,                -- :: (Read a) => String -> a
   readParen,           -- :: Bool -> ReadS a -> ReadS a
   lex,                 -- :: ReadS String

   module Text.ParserCombinators.ReadPrec,
   L.Lexeme(..),
   lexP,                -- :: ReadPrec Lexeme
   parens,              -- :: ReadPrec a -> ReadPrec a

   readListDefault,     -- :: Read a => ReadS [a]
   readListPrecDefault, -- :: Read a => ReadPrec [a]

) where

import Text.ParserCombinators.ReadPrec
import qualified Text.Read.Lex as L

import GHC.Base
import GHC.Read
import Data.Either
import Text.ParserCombinators.ReadP as P

-- | equivalent to 'readsPrec' with a precedence of 0.
reads :: Read a => ReadS a
reads = readsPrec minPrec

readEither :: Read a => String -> Either String a
readEither s =
  case [ x | (x,"") <- readPrec_to_S read' minPrec s ] of
    [x] -> Right x
    []  -> Left "Prelude.read: no parse"
    _   -> Left "Prelude.read: ambiguous parse"
 where
  read' =
    do x <- readPrec
       lift P.skipSpaces
       return x

-- | The 'read' function reads input from a string, which must be
-- completely consumed by the input process.
read :: Read a => String -> a
read s = either error id (readEither s)
