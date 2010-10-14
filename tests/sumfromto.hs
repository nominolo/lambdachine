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
{-  # NOINLINE plusInt #-}

minusInt :: Int -> Int -> Int
minusInt (I# m) (I# n) = I# (m -# n)
{-  # NOINLINE minusInt #-}

gtInt :: Int -> Int -> Bool
gtInt (I# m) (I# n) = m ># n
{-  # NOINLINE gtInt #-}

data List a = Nil | Cons a (List a)

upto :: Int -> Int -> List Int
upto from to =
  if from `gtInt` to then
    Nil
   else
    Cons from (upto (from `plusInt` I# 1#) to)

sum :: Int -> List Int -> Int
sum acc Nil = acc
sum acc (Cons x xs) =
  let acc' = (acc `plusInt` x) in 
  acc' `seq` sum acc' xs

{-
replicate :: Int -> a -> List a
replicate n x =
  case n of 
    I# 0# -> Nil
    _ -> Cons x (replicate (n `minusInt` I# 1#) x)
-}
{-
replicate' n x
  | I# 0# `gtInt` n = Nil
  | True = Cons x (replicate' (n `minusInt` I# 1#) x)
-}
{-
append :: List a -> List a -> List a
append Nil xs = xs
append (Cons y ys) xs = Cons y (append ys xs)

foldr :: (a -> r -> r) -> r -> List a -> r
foldr c n Nil = n
foldr c n (Cons a r) = c a (foldr c n r)

concat :: List (List a) -> List a
concat xs = foldr append Nil xs
-}
--concatMap :: (a -> List b) -> List a -> List a
--concatMap f xs = foldr 

zero :: Int
zero = I# 0#

one :: Int
one = I# 1#

test :: Int
test = (sum (I# 0#) (upto (I# 1#) (I# 5#)))

test1 = decr (I# 5#) Nil

decr :: Int -> List Int -> List Int
decr n l =
  if n `gtInt` zero then
    let l' = Cons n l
        n' = n `minusInt` one
    in n' `seq` decr n' l'
   else l

foldl :: (a -> b -> a) -> a -> List b -> a
foldl f z Nil = z
foldl f z (Cons x xs) = foldl f (z `f` x) xs

test2 a b = 
  let x = Cons a y
      y = Cons b x
  in x
