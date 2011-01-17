{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.Classes where

import GHC.Bool
import GHC.Integer
import GHC.Prim

infix  4  ==, /= -- , <, <=, >=, >

-- | The 'Eq' class defines equality ('==') and inequality ('/=').
-- All the basic datatypes exported by the "Prelude" are instances of 'Eq',
-- and 'Eq' may be derived for any datatype whose constituents are also
-- instances of 'Eq'.
--
-- Minimal complete definition: either '==' or '/='.
--
class  Eq a  where
    (==), (/=)           :: a -> a -> Bool

    {-# INLINE (/=) #-}
    {-# INLINE (==) #-}
    x /= y               = not (x == y)
    x == y               = not (x /= y)

-- | Boolean \"not\"
not :: Bool -> Bool
not True  = False
not False = True
