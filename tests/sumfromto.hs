{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Test1 where

--import Prelude (Int, print)
--import GHC.Base (plusInt, gtInt)
import GHC.Prim
import GHC.Bool

data Int = I# Int#

-- The NOINLINE stuff is to simulate these things coming from another
-- module (or package even).

plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)
{-# NOINLINE plusInt #-}

minusInt :: Int -> Int -> Int
minusInt (I# m) (I# n) = I# (m -# n)
{-# NOINLINE minusInt #-}

gtInt :: Int -> Int -> Bool
gtInt (I# m) (I# n) = m ># n
{-# NOINLINE gtInt #-}

data List a = Nil | Cons a (List a)

enumFromTo :: Int -> Int -> List Int
enumFromTo from to =
  if from `gtInt` to then
    Nil
   else
    Cons from (enumFromTo (from `plusInt` I# 1#) to)

sum :: List Int -> Int
sum Nil = I# 0#
sum (Cons x xs) = x `plusInt` sum xs

replicate :: Int -> a -> List a
replicate n a =
  case n of 
    I# 0# -> Nil
    _ -> Cons a (replicate (n `minusInt` I# 1#) a)

test = (sum (enumFromTo (I# 0#) (I# 10#)))
