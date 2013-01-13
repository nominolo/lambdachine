{-# LANGUAGE MagicHash, NoImplicitPrelude, Rank2Types #-}
module GHC.Base
  ( module GHC.Base
  , module GHC.Bool
  , module GHC.Types
  , module GHC.Classes
  , module GHC.Ordering
  , module GHC.Prim
  , module GHC.Err
  )
where

import GHC.Prim
import GHC.Types
import GHC.Bool
import GHC.Classes
import GHC.Ordering
import {-# SOURCE #-} GHC.Show
import {-# SOURCE #-} GHC.Err

import GHC.Tuple ()
import GHC.Unit ()

infixr 9  .
infixr 5  ++
infixl 4  <$
infixl 1  >>, >>=
infixr 0  $

-- | Identity function.
id                      :: a -> a
id x                    =  x

-- | Function composition.
{-# INLINE (.) #-}
-- Make sure it has TWO args only on the left, so that it inlines
-- when applied to two functions, even if there is no final argument
(.)    :: (b -> c) -> (a -> b) -> a -> c
(.) f g = \x -> f (g x)

-- | Application operator.  This operator is redundant, since ordinary
-- application @(f x)@ means the same as @(f '$' x)@. However, '$' has
-- low, right-associative binding precedence, so it sometimes allows
-- parentheses to be omitted; for example:
--
-- >     f $ g $ h x  =  f (g (h x))
--
-- It is also useful in higher-order situations, such as @'map' ('$' 0) xs@,
-- or @'Data.List.zipWith' ('$') fs xs@.
{-# INLINE ($) #-}
($)                     :: (a -> b) -> a -> b
f $ x                   =  f x

-- | Constant function.
const                   :: a -> b -> a
const x _               =  x

-- | @'flip' f@ takes its (first) two arguments in the reverse order of @f@.
flip                    :: (a -> b -> c) -> b -> a -> c
flip f x y              =  f y x


-- | 'asTypeOf' is a type-restricted version of 'const'.  It is usually
-- used as an infix operator, and its typing forces its first argument
-- (which is usually overloaded) to have the same type as the second.
asTypeOf                :: a -> a -> a
asTypeOf                =  const


class  Functor f  where
    fmap        :: (a -> b) -> f a -> f b

    -- | Replace all locations in the input with the same value.
    -- The default definition is @'fmap' . 'const'@, but this may be
    -- overridden with a more efficient version.
    (<$)        :: a -> f b -> f a
    (<$)        =  fmap . const

class  Monad m  where
    -- | Sequentially compose two actions, passing any value produced
    -- by the first as an argument to the second.
    (>>=)       :: m a -> (a -> m b) -> m b
    -- | Sequentially compose two actions, discarding any value produced
    -- by the first, like sequencing operators (such as the semicolon)
    -- in imperative languages.
    (>>)        :: m a -> m b -> m b
        -- Explicit for-alls so that we know what order to
        -- give type arguments when desugaring

    -- | Inject a value into the monadic type.
    return      :: a -> m a
    -- | Fail with a message.  This operation is not part of the
    -- mathematical definition of a monad, but is invoked on pattern-match
    -- failure in a @do@ expression.
    fail        :: String -> m a

    {-# INLINE (>>) #-}
    m >> k      = m >>= \_ -> k
    fail s      = error s

instance Functor [] where
    fmap = map

instance  Monad []  where
    m >>= k             = foldr ((++) . k) [] m
    m >> k              = foldr ((++) . (\ _ -> k)) [] m
    return x            = [x]
    fail _              = []

type String = [Char]

-- error :: String -> a
-- error = error

{-# INLINE eqInt #-}
{-# INLINE neInt #-}
{-# INLINE gtInt #-}
{-# INLINE geInt #-}
{-# INLINE ltInt #-}
{-# INLINE leInt #-}

gtInt, geInt, eqInt, neInt, ltInt, leInt :: Int -> Int -> Bool
(I# x) `gtInt` (I# y) = x >#  y
(I# x) `geInt` (I# y) = x >=# y
(I# x) `eqInt` (I# y) = x ==# y
(I# x) `neInt` (I# y) = x /=# y
(I# x) `ltInt` (I# y) = x <#  y
(I# x) `leInt` (I# y) = x <=# y

instance Eq Int where
  (==) = eqInt
  (/=) = neInt

instance Ord Int where
    compare = compareInt
    (<)     = ltInt
    (<=)    = leInt
    (>=)    = geInt
    (>)     = gtInt

compareInt :: Int -> Int -> Ordering
(I# x#) `compareInt` (I# y#) = compareInt# x# y#

compareInt# :: Int# -> Int# -> Ordering
compareInt# x# y#
    | x# <#  y# = LT
    | x# ==# y# = EQ
    | otherwise = GT


{-# INLINE plusInt #-}
{-# INLINE minusInt #-}
{-# INLINE timesInt #-}
{-# INLINE negateInt #-}

plusInt, minusInt, timesInt, modInt :: Int -> Int -> Int
(I# x) `plusInt`  (I# y) = I# (x +# y)
(I# x) `minusInt` (I# y) = I# (x -# y)
(I# x) `timesInt` (I# y) = I# (x *# y)
(I# x) `modInt`   (I# y) = I# (x `modInt#` y)
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)

-- XXX: Not quite correct, might overflow
negateInt :: Int -> Int
negateInt (I# n) = I# (negateInt# n)

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
  | (x# ># 0#) && (y# <# 0#) ||
    (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
  | otherwise                   = r#
 where
   !r# = x# `remInt#` y#

divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    | (x# ># 0#) && (y# <# 0#) = ((x# -# 1#) `quotInt#` y#) -# 1#
    | (x# <# 0#) && (y# ># 0#) = ((x# +# 1#) `quotInt#` y#) -# 1#
    | otherwise                = x# `quotInt#` y#


{-
quotInt, remInt, divInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
-}

(++) :: [a] -> [a] -> [a]
(++) []     ys = ys
(++) (x:xs) ys = x : xs ++ ys

-- |'otherwise' is defined as the value 'True'.  It helps to make
-- guards more readable.  eg.
--
-- >  f x | x < 0     = ...
-- >      | otherwise = ...
otherwise               :: Bool
otherwise               =  True

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z = go
  where
    go []     = z
    go (y:ys) = y `k` go ys

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g k []
 where k x xs = x : xs
       {-# NOINLINE k #-}

augment :: forall a. (forall b. (a->b->b) -> b -> b) -> [a] -> [a]
{-# INLINE [1] augment #-}
augment g xs = g (:) xs

{-# RULES
"fold/build"    forall k z (g::forall b. (a->b->b) -> b -> b) . 
                foldr k z (build g) = g k z

"foldr/augment" forall k z xs (g::forall b. (a->b->b) -> b -> b) . 
                foldr k z (augment g xs) = g k (foldr k z xs)

"foldr/id"                        foldr (:) [] = \x  -> x
 #-}

chr :: Int -> Char
chr i@(I# i#)
 | int2Word# i# `leWord#` int2Word# 0x10FFFF# = C# (chr# i#)
 | otherwise
    = error ("Prelude.chr: bad argument: " ++ showSignedInt (I# 9#) i "")


unsafeChr :: Int -> Char
unsafeChr (I# i#) = C# (chr# i#)

-- | The 'Prelude.fromEnum' method restricted to the type 'Data.Char.Char'.
ord :: Char -> Int
ord (C# c#) = I# (ord# c#)

eqString :: String -> String -> Bool
eqString []       []       = True
eqString (c1:cs1) (c2:cs2) = c1 == c2 && cs1 `eqString` cs2
eqString _        _        = False

-- This code is needed for virtually all programs, since it's used for
-- unpacking the strings of error messages.

unpackCString# :: Addr# -> [Char]
{-# NOINLINE unpackCString# #-}
    -- There's really no point in inlining this, ever, cos
    -- the loop doesn't specialise in an interesting
    -- But it's pretty small, so there's a danger that
    -- it'll be inlined at every literal, which is a waste
unpackCString# addr = unpack 0#
 where
   unpack nh
     | ch `eqChar#` '\0'# = []
     | otherwise          = C# ch : unpack (nh +# 1#)
     where
       !ch = indexCharOffAddr# addr nh

unpackAppendCString# :: Addr# -> [Char] -> [Char]
{-# NOINLINE unpackAppendCString# #-}
     -- See the NOINLINE note on unpackCString# 
unpackAppendCString# addr rest
  = unpack 0#
  where
    unpack nh
      | ch `eqChar#` '\0'# = rest
      | otherwise          = C# ch : unpack (nh +# 1#)
      where
        !ch = indexCharOffAddr# addr nh


{- Seems clumsy. Should perhaps put minInt and MaxInt directly into MachDeps.h -}
minInt  = I# (-0x8000000000000000#)
maxInt  = I# 0x7FFFFFFFFFFFFFFF#

zeroInt, oneInt, twoInt, maxInt, minInt :: Int
zeroInt = I# 0#
oneInt  = I# 1#
twoInt  = I# 2#

-- | @'until' p f@ yields the result of applying @f@ until @p@ holds.
until                   :: (a -> Bool) -> (a -> a) -> a -> a
until p f x | p x       =  x
            | otherwise =  until p f (f x)
