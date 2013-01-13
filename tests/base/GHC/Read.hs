{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Read
  ( Read(..)   -- class

  -- ReadS type
  , ReadS      -- :: *; = String -> [(a,String)]

  -- H98 compatibility
  -- , lex         -- :: ReadS String
  -- , lexLitChar  -- :: ReadS String
  -- , readLitChar -- :: ReadS Char
  -- , lexDigits   -- :: ReadS String

  -- defining readers
  -- , lexP       -- :: ReadPrec Lexeme
  , paren      -- :: ReadPrec a -> ReadPrec a
  , parens     -- :: ReadPrec a -> ReadPrec a
  , list       -- :: ReadPrec a -> ReadPrec [a]
  , choose     -- :: [(String, ReadPrec a)] -> ReadPrec a
  , readListDefault, readListPrecDefault

  -- Temporary
  , readParen

  -- XXX Can this be removed?
  , readp
  )
 where

import qualified Text.ParserCombinators.ReadP as P

import Text.ParserCombinators.ReadP ( ReadP, ReadS, readP_to_S )

class Read a where
  readsPrec    :: Int -> ReadS a
  readList     :: ReadS [a]
  readPrec     :: ReadPrec a
  readListPrec :: ReadPrec [a]
  
  -- default definitions
  readsPrec    = readPrec_to_S readPrec
  readList     = readPrec_to_S (list readPrec) 0
  readPrec     = readS_to_Prec readsPrec
  readListPrec = readS_to_Prec (\_ -> readList)

list :: ReadPrec a -> ReadPrec [a]
