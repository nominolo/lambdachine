{-# LANGUAGE MagicHash, NoImplicitPrelude, Rank2Types #-}
module GHC.Base
  ( module GHC.Base
  , module GHC.Bool
  , module GHC.Types
  , module GHC.Classes
  )
where

import GHC.Prim
import GHC.Types
import GHC.Bool
import GHC.Classes
import GHC.Ordering

infixr 5  ++
infixl 1  >>, >>=

-- | Identity function.
id                      :: a -> a
id x                    =  x

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

type String = [Char]

error :: String -> a
error = error

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

-- XXX: Not quite correct, might overflow
negateInt :: Int -> Int
negateInt (I# n) = I# (0# -# n)

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
  | (x# ># 0#) && (y# <# 0#) ||
    (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
  | otherwise                   = r#
 where
   !r# = x# `remInt#` y#

divInt :: Int -> Int -> Int
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)

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
