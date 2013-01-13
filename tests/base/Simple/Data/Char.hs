{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Simple.Data.Char
    (
      Char

    , String

    -- * Character classification
    -- | Unicode characters are divided into letters, numbers, marks,
    -- punctuation, symbols, separators (including spaces) and others
    -- (including control characters).
    , isControl, isSpace
    , isLower, isUpper, isAlpha, isAlphaNum, isPrint
    , isDigit, isOctDigit, isHexDigit
--    , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator

    -- ** Subranges
    , isAscii, isLatin1
    , isAsciiUpper, isAsciiLower

    -- ** Unicode general categories
    -- , GeneralCategory(..), generalCategory

    -- * Case conversion
    , toUpper, toLower -- , toTitle  -- :: Char -> Char

    -- * Single digit characters
    , digitToInt        -- :: Char -> Int
    , intToDigit        -- :: Int  -> Char

    -- * Numeric representations
    , ord               -- :: Char -> Int
    , chr               -- :: Int  -> Char

    -- * String representations
    -- , showLitChar       -- :: Char -> ShowS
    -- , lexLitChar        -- :: ReadS String
    -- , readLitChar       -- :: ReadS Char 

     -- Implementation checked wrt. Haskell 98 lib report, 1/99.
    ) where

import GHC.Base
import GHC.Show
import GHC.Unicode
import GHC.Num
import GHC.Enum

-- | Convert a single digit 'Char' to the corresponding 'Int'.  
-- This function fails unless its argument satisfies 'isHexDigit',
-- but recognises both upper and lower-case hexadecimal digits
-- (i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@).
digitToInt :: Char -> Int
digitToInt c
 | isDigit c            =  ord c - ord '0'
 | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
 | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
 | otherwise            =  error ("Char.digitToInt: not a digit " ++ show c) -- sigh
