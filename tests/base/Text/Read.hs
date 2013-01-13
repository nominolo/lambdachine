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
