{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
module GHC.Num (module GHC.Num, module GHC.Integer) where

import GHC.Base
--import GHC.Enum
--import GHC.Show
import GHC.Integer

infixl 7  *
infixl 6  +, -

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

class  (Eq a) => Num a  where
    (+), (-), (*)       :: a -> a -> a
    -- | Unary negation.
    negate              :: a -> a
    -- | Absolute value.
    abs                 :: a -> a
    -- | Sign of a number.
    -- The functions 'abs' and 'signum' should satisfy the law:
    --
    -- > abs x * signum x == x
    --
    -- For real numbers, the 'signum' is either @-1@ (negative), @0@ (zero)
    -- or @1@ (positive).
    signum              :: a -> a
    -- | Conversion from an 'Integer'.
    -- An integer literal represents the application of the function
    -- 'fromInteger' to the appropriate value of type 'Integer',
    -- so such literals have type @('Num' a) => a@.
    fromInteger         :: Integer -> a

    {-# INLINE (-) #-}
    {-# INLINE negate #-}
    x - y               = x + negate y
    negate x            = 0 - x

-- | the same as @'flip' ('-')@.
--
-- Because @-@ is treated specially in the Haskell grammar,
-- @(-@ /e/@)@ is not a section, but an application of prefix negation.
-- However, @('subtract'@ /exp/@)@ is equivalent to the disallowed section.
{-# INLINE subtract #-}
subtract :: (Num a) => a -> a -> a
subtract x y = y - x

instance  Num Int  where
    (+)    = plusInt
    (-)    = minusInt
    negate = negateInt
    (*)    = timesInt
    abs n  = if n `geInt` 0 then n else negateInt n

    signum n = 
      if n `ltInt` 0 then negateInt 1 else
        if n `eqInt` 0 then 0 else 1

    {-# INLINE fromInteger #-}	 -- Just to be sure!
    fromInteger i = I# (toInt# i)
