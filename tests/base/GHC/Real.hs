{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
module GHC.Real where

import GHC.Base
import GHC.Num
import GHC.Enum
import GHC.Show

-- infixr 8  ^, ^^
infixl 7  /, `quot`, `rem`, `div`, `mod`
infixl 7  %

default ()              -- Double isn't available yet,
                        -- and we shouldn't be using defaults anyway

data  (Integral a)      => Ratio a = !a :% !a  deriving (Eq)

type  Rational          =  Ratio Integer

ratioPrec, ratioPrec1 :: Int
ratioPrec  = 7  -- Precedence of ':%' constructor
ratioPrec1 = ratioPrec + 1

infinity, notANumber :: Rational
infinity   = 1 :% 0
notANumber = 0 :% 0
-- Use :%, not % for Inf/NaN; the latter would
-- immediately lead to a runtime error, because it normalises.

-- | Forms the ratio of two integral numbers.
(%)                     :: (Integral a) => a -> a -> Ratio a
x % y                   =  reduce (x * signum y) (abs y)

-- | Extract the numerator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
numerator       :: (Integral a) => Ratio a -> a
numerator   (x :% _)    =  x

-- | Extract the denominator of the ratio in reduced form:
-- the numerator and denominator have no common factor and the denominator
-- is positive.
denominator     :: (Integral a) => Ratio a -> a
denominator (_ :% y)    =  y


reduce ::  (Integral a) => a -> a -> Ratio a
reduce _ 0              =  error "Ratio.%: zero denominator"
reduce x y              =  (x `quot` d) :% (y `quot` d)
                           where d = gcd x y


class  (Num a, Ord a) => Real a  where
    -- | the rational equivalent of its real argument with full precision
    toRational          ::  a -> Rational


-- | Integral numbers, supporting integer division.
--
-- Minimal complete definition: 'quotRem' and 'toInteger'
class  (Real a, Enum a) => Integral a  where
    -- | integer division truncated toward zero
    quot                :: a -> a -> a
    -- | integer remainder, satisfying
    --
    -- > (x `quot` y)*y + (x `rem` y) == x
    rem                 :: a -> a -> a
    -- | integer division truncated toward negative infinity
    div                 :: a -> a -> a
    -- | integer modulus, satisfying
    --
    -- > (x `div` y)*y + (x `mod` y) == x
    mod                 :: a -> a -> a
    -- | simultaneous 'quot' and 'rem'
    quotRem             :: a -> a -> (a,a)
    -- | simultaneous 'div' and 'mod'
    divMod              :: a -> a -> (a,a)
    -- | conversion to 'Integer'
    toInteger           :: a -> Integer

    {-# INLINE quot #-}
    {-# INLINE rem #-}
    {-# INLINE div #-}
    {-# INLINE mod #-}
    n `quot` d          =  q  where (q,_) = quotRem n d
    n `rem` d           =  r  where (_,r) = quotRem n d
    n `div` d           =  q  where (q,_) = divMod n d
    n `mod` d           =  r  where (_,r) = divMod n d

    divMod n d          =  if signum r == negate (signum d) then (q-1, r+d) else qr
                           where qr@(q,r) = quotRem n d

-- | Fractional numbers, supporting real division.
--
-- Minimal complete definition: 'fromRational' and ('recip' or @('/')@)
class  (Num a) => Fractional a  where
    -- | fractional division
    (/)                 :: a -> a -> a
    -- | reciprocal fraction
    recip               :: a -> a
    -- | Conversion from a 'Rational' (that is @'Ratio' 'Integer'@).
    -- A floating literal stands for an application of 'fromRational'
    -- to a value of type 'Rational', so such literals have type
    -- @('Fractional' a) => a@.
    fromRational        :: Rational -> a

    {-# INLINE recip #-}
    {-# INLINE (/) #-}
    recip x             =  1 / x
    x / y               = x * recip y

-- | Extracting components of fractions.
--
-- Minimal complete definition: 'properFraction'
class  (Real a, Fractional a) => RealFrac a  where
    -- | The function 'properFraction' takes a real fractional number @x@
    -- and returns a pair @(n,f)@ such that @x = n+f@, and:
    --
    -- * @n@ is an integral number with the same sign as @x@; and
    --
    -- * @f@ is a fraction with the same type and sign as @x@,
    --   and with absolute value less than @1@.
    --
    -- The default definitions of the 'ceiling', 'floor', 'truncate'
    -- and 'round' functions are in terms of 'properFraction'.
    properFraction      :: (Integral b) => a -> (b,a)
    -- | @'truncate' x@ returns the integer nearest @x@ between zero and @x@
    truncate            :: (Integral b) => a -> b
    -- | @'round' x@ returns the nearest integer to @x@;
    --   the even integer if @x@ is equidistant between two integers
    round               :: (Integral b) => a -> b
    -- | @'ceiling' x@ returns the least integer not less than @x@
    ceiling             :: (Integral b) => a -> b
    -- | @'floor' x@ returns the greatest integer not greater than @x@
    floor               :: (Integral b) => a -> b

    {-# INLINE truncate #-}
    truncate x          =  m  where (m,_) = properFraction x

    round x             =  let (n,r) = properFraction x
                               m     = if r < 0 then n - 1 else n + 1
                           in case signum (abs r - 0.5) of
                                -1 -> n
                                0  -> if even n then n else m
                                1  -> m
                                _  -> error "round default defn: Bad value"

    ceiling x           =  if r > 0 then n + 1 else n
                           where (n,r) = properFraction x

    floor x             =  if r < 0 then n - 1 else n
                           where (n,r) = properFraction x



instance  Real Int  where
    toRational x        =  toInteger x % 1

instance  Integral Int  where
    toInteger (I# i) = smallInteger i

    a `quot` b
     | b == 0                     = divZeroError
     | a == minBound && b == (-1) = overflowError
     | otherwise                  =  a `quotInt` b

    a `rem` b
     | b == 0                     = divZeroError
     | a == minBound && b == (-1) = overflowError
     | otherwise                  =  a `remInt` b

    a `div` b
     | b == 0                     = divZeroError
     | a == minBound && b == (-1) = overflowError
     | otherwise                  =  a `divInt` b

    a `mod` b
     | b == 0                     = divZeroError
     | a == minBound && b == (-1) = overflowError
     | otherwise                  =  a `modInt` b

    a `quotRem` b
     | b == 0                     = divZeroError
     | a == minBound && b == (-1) = overflowError
     | otherwise                  =  a `quotRemInt` b

    a `divMod` b
     | b == 0                     = divZeroError
     | a == minBound && b == (-1) = overflowError
     | otherwise                  =  a `divModInt` b

instance Real Integer where
  toRational x        =  x % 1

instance Integral Integer where
  toInteger n      = n

  _ `quot` 0 = divZeroError
  n `quot` d = n `quotInteger` d

  _ `rem` 0 = divZeroError
  n `rem`  d = n `remInteger`  d

  _ `divMod` 0 = divZeroError
  a `divMod` b = case a `divModInteger` b of
                 (# x, y #) -> (x, y)

  _ `quotRem` 0 = divZeroError
  a `quotRem` b = case a `quotRemInteger` b of
                  (# q, r #) -> (q, r)

fromIntegral :: (Integral a, Num b) => a -> b
fromIntegral = fromInteger . toInteger

even, odd       :: (Integral a) => a -> Bool
even n          =  n `rem` 2 == 0
odd             =  not . even

-- | @'gcd' x y@ is the greatest (positive) integer that divides both @x@
-- and @y@; for example @'gcd' (-3) 6@ = @3@, @'gcd' (-3) (-6)@ = @3@,
-- @'gcd' 0 4@ = @4@.  @'gcd' 0 0@ raises a runtime error.
gcd             :: (Integral a) => a -> a -> a
gcd 0 0         =  error "Prelude.gcd: gcd 0 0 is undefined"
gcd x y         =  gcd' (abs x) (abs y)
                   where gcd' a 0  =  a
                         gcd' a b  =  gcd' b (a `rem` b)

-- | @'lcm' x y@ is the smallest positive integer that both @x@ and @y@ divide.
lcm             :: (Integral a) => a -> a -> a
{-# SPECIALISE lcm :: Int -> Int -> Int #-}
lcm _ 0         =  0
lcm 0 _         =  0
lcm x y         =  abs ((x `quot` (gcd x y)) * y)
