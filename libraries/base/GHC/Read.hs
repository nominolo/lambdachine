{-# LANGUAGE NoImplicitPrelude #-}
module GHC.Read
  ( Read(..)   -- class

  -- ReadS type
  , ReadS      -- :: *; = String -> [(a,String)]

  -- H98 compatibility
  , lex         -- :: ReadS String
  , lexLitChar  -- :: ReadS String
  , readLitChar -- :: ReadS Char
  , lexDigits   -- :: ReadS String

  -- defining readers
  , lexP       -- :: ReadPrec Lexeme
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
import qualified Text.Read.Lex as L
import Text.ParserCombinators.ReadP ( ReadP, ReadS, readP_to_S )
import Text.ParserCombinators.ReadPrec
import Data.Maybe
import {-# SOURCE #-} GHC.Unicode       ( isDigit )
import GHC.Num
import GHC.Real
-- import GHC.Float ()
import GHC.Show
import GHC.Base
-- import GHC.Arr

readParen       :: Bool -> ReadS a -> ReadS a
-- A Haskell 98 function
readParen b g   =  if b then mandatory else optional
                   where optional r  = g r ++ mandatory r
                         mandatory r = do
                                ("(",s) <- lex r
                                (x,t)   <- optional s
                                (")",u) <- lex t
                                return (x,u)

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

readListDefault :: Read a => ReadS [a]
readListDefault = readPrec_to_S readListPrec 0

readListPrecDefault :: Read a => ReadPrec [a]
readListPrecDefault = list readPrec

lex :: ReadS String             -- As defined by H98
lex s  = readP_to_S L.hsLex s

lexLitChar :: ReadS String      -- As defined by H98
lexLitChar = readP_to_S (do { (s, _) <- P.gather L.lexChar ;
                              return s })
        -- There was a skipSpaces before the P.gather L.lexChar,
        -- but that seems inconsistent with readLitChar

readLitChar :: ReadS Char       -- As defined by H98
readLitChar = readP_to_S L.lexChar

lexDigits :: ReadS String
lexDigits = readP_to_S (P.munch1 isDigit)

lexP :: ReadPrec L.Lexeme
lexP = lift L.lex

paren :: ReadPrec a -> ReadPrec a
paren p = do L.Punc "(" <- lexP
             x          <- reset p
             L.Punc ")" <- lexP
             return x

parens :: ReadPrec a -> ReadPrec a
parens p = optional
 where
  optional  = p +++ mandatory
  mandatory = paren optional

list :: ReadPrec a -> ReadPrec [a]
list readx =
  parens
  ( do L.Punc "[" <- lexP
       (listRest False +++ listNext)
  )
 where
  listRest started =
    do L.Punc c <- lexP
       case c of
         "]"           -> return []
         "," | started -> listNext
         _             -> pfail
  
  listNext =
    do x  <- reset readx
       xs <- listRest True
       return (x:xs)

choose :: [(String, ReadPrec a)] -> ReadPrec a
choose sps = foldr ((+++) . try_one) pfail sps
           where
             try_one (s,p) = do { token <- lexP ;
                                  case token of
                                    L.Ident s'  | s==s' -> p
                                    L.Symbol s' | s==s' -> p
                                    _other              -> pfail }

instance Read Char where
  readPrec =
    parens
    ( do L.Char c <- lexP
         return c
    )

  readListPrec =
    parens
    ( do L.String s <- lexP     -- Looks for "foo"
         return s
     +++
      readListPrecDefault       -- Looks for ['f','o','o']
    )                           -- (more generous than H98 spec)

  readList = readListDefault

instance Read Bool where
  readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "True"  -> return True
           "False" -> return False
           _       -> pfail
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Ordering where
  readPrec =
    parens
    ( do L.Ident s <- lexP
         case s of
           "LT" -> return LT
           "EQ" -> return EQ
           "GT" -> return GT
           _    -> pfail
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read a => Read (Maybe a) where
  readPrec =
    parens
    (do L.Ident "Nothing" <- lexP
        return Nothing
     +++
     prec appPrec (
        do L.Ident "Just" <- lexP
           x              <- step readPrec
           return (Just x))
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read a => Read [a] where
  readPrec     = readListPrec
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- instance  (Ix a, Read a, Read b) => Read (Array a b)  where
--     readPrec = parens $ prec appPrec $
--                do L.Ident "array" <- lexP
--                   theBounds <- step readPrec
--                   vals   <- step readPrec
--                   return (array theBounds vals)

--     readListPrec = readListPrecDefault
--     readList     = readListDefault

instance Read L.Lexeme where
  readPrec     = lexP
  readListPrec = readListPrecDefault
  readList     = readListDefault

readNumber :: Num a => (L.Lexeme -> ReadPrec a) -> ReadPrec a
-- Read a signed number
readNumber convert =
  parens
  ( do x <- lexP
       case x of
         L.Symbol "-" -> do y <- lexP
                            n <- convert y
                            return (negate n)

         _   -> convert x
  )


convertInt :: Num a => L.Lexeme -> ReadPrec a
convertInt (L.Int i) = return (fromInteger i)
convertInt _         = pfail

convertFrac :: Fractional a => L.Lexeme -> ReadPrec a
convertFrac (L.Int i) = return (fromInteger i)
convertFrac (L.Rat r) = return (fromRational r)
convertFrac _         = pfail

instance Read Int where
  readPrec     = readNumber convertInt
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read Integer where
  readPrec     = readNumber convertInt
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- instance Read Float where
--   readPrec     = readNumber convertFrac
--   readListPrec = readListPrecDefault
--   readList     = readListDefault

-- instance Read Double where
--   readPrec     = readNumber convertFrac
--   readListPrec = readListPrecDefault
--   readList     = readListDefault

instance (Integral a, Read a) => Read (Ratio a) where
  readPrec =
    parens
    ( prec ratioPrec
      ( do x            <- step readPrec
           L.Symbol "%" <- lexP
           y            <- step readPrec
           return (x % y)
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance Read () where
  readPrec =
    parens
    ( paren
      ( return ()
      )
    )

  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b) => Read (a,b) where
  readPrec = wrap_tup read_tup2
  readListPrec = readListPrecDefault
  readList     = readListDefault

wrap_tup :: ReadPrec a -> ReadPrec a
wrap_tup p = parens (paren p)

read_comma :: ReadPrec ()
read_comma = do { L.Punc "," <- lexP; return () }

read_tup2 :: (Read a, Read b) => ReadPrec (a,b)
-- Reads "a , b"  no parens!
read_tup2 = do x <- readPrec
               read_comma
               y <- readPrec
               return (x,y)

read_tup4 :: (Read a, Read b, Read c, Read d) => ReadPrec (a,b,c,d)
read_tup4 = do  (a,b) <- read_tup2
                read_comma
                (c,d) <- read_tup2
                return (a,b,c,d)


read_tup8 :: (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)
          => ReadPrec (a,b,c,d,e,f,g,h)
read_tup8 = do  (a,b,c,d) <- read_tup4
                read_comma
                (e,f,g,h) <- read_tup4
                return (a,b,c,d,e,f,g,h)


instance (Read a, Read b, Read c) => Read (a, b, c) where
  readPrec = wrap_tup (do { (a,b) <- read_tup2; read_comma 
                          ; c <- readPrec 
                          ; return (a,b,c) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d) => Read (a, b, c, d) where
  readPrec = wrap_tup read_tup4
  readListPrec = readListPrecDefault
  readList     = readListDefault

{-
instance (Read a, Read b, Read c, Read d, Read e) => Read (a, b, c, d, e) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; e <- readPrec
                          ; return (a,b,c,d,e) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f)
        => Read (a, b, c, d, e, f) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; (e,f) <- read_tup2
                          ; return (a,b,c,d,e,f) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g)
        => Read (a, b, c, d, e, f, g) where
  readPrec = wrap_tup (do { (a,b,c,d) <- read_tup4; read_comma
                          ; (e,f) <- read_tup2; read_comma
                          ; g <- readPrec
                          ; return (a,b,c,d,e,f,g) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h)
        => Read (a, b, c, d, e, f, g, h) where
  readPrec     = wrap_tup read_tup8
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i)
        => Read (a, b, c, d, e, f, g, h, i) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; i <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j)
        => Read (a, b, c, d, e, f, g, h, i, j) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j) <- read_tup2
                          ; return (a,b,c,d,e,f,g,h,i,j) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k)
        => Read (a, b, c, d, e, f, g, h, i, j, k) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j) <- read_tup2; read_comma
                          ; k <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; m <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m, Read n)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; (m,n) <- read_tup2
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

instance (Read a, Read b, Read c, Read d, Read e, Read f, Read g, Read h,
          Read i, Read j, Read k, Read l, Read m, Read n, Read o)
        => Read (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) where
  readPrec = wrap_tup (do { (a,b,c,d,e,f,g,h) <- read_tup8; read_comma
                          ; (i,j,k,l) <- read_tup4; read_comma
                          ; (m,n) <- read_tup2; read_comma
                          ; o <- readPrec
                          ; return (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o) })
  readListPrec = readListPrecDefault
  readList     = readListDefault

-- -}

readp :: Read a => ReadP a
readp = readPrec_to_P readPrec minPrec
