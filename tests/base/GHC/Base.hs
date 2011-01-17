{-# LANGUAGE MagicHash, NoImplicitPrelude #-}
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

-- | Identity function.
id                      :: a -> a
id x                    =  x

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

{-# INLINE plusInt #-}
{-# INLINE minusInt #-}
{-# INLINE timesInt #-}

plusInt, minusInt, timesInt:: Int -> Int -> Int
(I# x) `plusInt`  (I# y) = I# (x +# y)
(I# x) `minusInt` (I# y) = I# (x -# y)
(I# x) `timesInt` (I# y) = I# (x *# y)


{-
quotInt, remInt, divInt, modInt :: Int -> Int -> Int
(I# x) `quotInt`  (I# y) = I# (x `quotInt#` y)
(I# x) `remInt`   (I# y) = I# (x `remInt#`  y)
(I# x) `divInt`   (I# y) = I# (x `divInt#`  y)
(I# x) `modInt`   (I# y) = I# (x `modInt#`  y)
-}