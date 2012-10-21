{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
-- #hide
module GHC.Unicode (
    isAscii, isLatin1, isControl,
    isAsciiUpper, isAsciiLower,
    isPrint, isSpace,  isUpper,
    isLower, isAlpha,  isDigit,
    isOctDigit, isHexDigit, isAlphaNum,
    toUpper, toLower, -- toTitle,
--    wgencat,
  ) where

import GHC.Base

-- | Selects the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.
isAscii                 :: Char -> Bool
isAscii c               =  c <  '\x80'

-- | Selects the first 256 characters of the Unicode character set,
-- corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1                :: Char -> Bool
isLatin1 c              =  c <= '\xff'

-- | Selects ASCII lower-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isLower'.
isAsciiLower :: Char -> Bool
isAsciiLower c          =  c >= 'a' && c <= 'z'

-- | Selects ASCII upper-case letters,
-- i.e. characters satisfying both 'isAscii' and 'isUpper'.
isAsciiUpper :: Char -> Bool
isAsciiUpper c          =  c >= 'A' && c <= 'Z'


-- | Returns 'True' for any Unicode space character, and the control
-- characters @\\t@, @\\n@, @\\r@, @\\f@, @\\v@.
isSpace                 :: Char -> Bool
-- isSpace includes non-breaking space
-- Done with explicit equalities both for efficiency, and to avoid a tiresome
-- recursion with GHC.List elem
isSpace c               =  c == ' '     ||
                           c == '\t'    ||
                           c == '\n'    ||
                           c == '\r'    ||
                           c == '\f'    ||
                           c == '\v'    ||
                           c == '\xa0'  {- ||
                           iswspace (fromIntegral (ord c)) /= 0 -}

-- No unicode for the time being -----------------------------------------

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- Title case is used by a small number of letter ligatures like the
-- single-character form of /Lj/.
isUpper                 :: Char -> Bool
-- The upper case ISO characters have the multiplication sign dumped
-- randomly in the middle of the range.  Go figure.
isUpper c               =  c >= 'A' && c <= 'Z' ||
                           c >= '\xC0' && c <= '\xD6' ||
                           c >= '\xD8' && c <= '\xDE'

-- | Selects lower-case alphabetic Unicode characters (letters).
isLower                 :: Char -> Bool
isLower c               =  c >= 'a' && c <= 'z' ||
                           c >= '\xDF' && c <= '\xF6' ||
                           c >= '\xF8' && c <= '\xFF'

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- title-case letters, plus letters of caseless scripts and modifiers letters).
-- This function is equivalent to 'Data.Char.isLetter'.
isAlpha                 :: Char -> Bool
isAlpha c               =  isLower c || isUpper c


isControl c             =  c < ' ' || c >= '\DEL' && c <= '\x9f'
isPrint c               =  not (isControl c)

-- The lower case ISO characters have the division sign dumped
-- randomly in the middle of the range.  Go figure.

-- | Selects alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are selected by this
-- function but not by 'isDigit'.  Such digits may be part of identifiers
-- but are not used by the printer and reader to represent numbers.
isAlphaNum              :: Char -> Bool
isAlphaNum c            =  isAlpha c || isDigit c

-- | Selects ASCII digits, i.e. @\'0\'@..@\'9\'@.
isDigit                 :: Char -> Bool
isDigit c               =  c >= '0' && c <= '9'

-- | Selects ASCII octal digits, i.e. @\'0\'@..@\'7\'@.
isOctDigit              :: Char -> Bool
isOctDigit c            =  c >= '0' && c <= '7'

-- | Selects ASCII hexadecimal digits,
-- i.e. @\'0\'@..@\'9\'@, @\'a\'@..@\'f\'@, @\'A\'@..@\'F\'@.
isHexDigit              :: Char -> Bool
isHexDigit c            =  isDigit c || c >= 'A' && c <= 'F' ||
                                        c >= 'a' && c <= 'f'

-- Case-changing operations

-- | Convert a letter to the corresponding upper-case letter, if any.
-- Any other character is returned unchanged.
toUpper                 :: Char -> Char
toUpper c@(C# c#)
  | isAsciiLower c    = C# (chr# (ord# c# -# 32#))
  | isAscii c         = c
    -- fall-through to the slower stuff.
  | isLower c   && c /= '\xDF' && c /= '\xFF'
  = unsafeChr (ord c `minusInt` ord 'a' `plusInt` ord 'A')
  | otherwise
  = c


-- | Convert a letter to the corresponding lower-case letter, if any.
-- Any other character is returned unchanged.
toLower                 :: Char -> Char
toLower c@(C# c#)
  | isAsciiUpper c = C# (chr# (ord# c# +# 32#))
  | isAscii c      = c
  | isUpper c      = unsafeChr (ord c `minusInt` ord 'A' `plusInt` ord 'a')
  | otherwise      =  c

