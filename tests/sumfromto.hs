{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
module Test1 where

--import Prelude (Int, print)
--import GHC.Base (plusInt, gtInt)
import GHC.Prim
import GHC.Bool
import GHC.Integer.GMP.Internals (Integer(..))
--import Prelude

data Int = I# Int#

--data Integer = S# Int#

smallInteger :: Int# -> Integer
smallInteger x = S# x
{-# INLINE smallInteger #-}

fromInteger :: Integer -> Int
fromInteger (S# n) = I# n
{-# INLINE fromInteger #-}

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

eqInt :: Int -> Int -> Bool
eqInt (I# m) (I# n) = m ==# n

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
test = (sum 1234567890 (upto (I# 1#) (I# 15#)))
--test = sum' (I# 4#)

test1 :: Int
test1 = sum zero (decr (I# 15#) Nil)

--test = f (I# 5#) A0

--data A = A1 A | A2 A
-- data B = B1 A 
{-
f :: Int -> A -> A
f x a =
  if x `gtInt` zero then 
    let a' = A1 x a
        x' = x `minusInt` one
    in x' `seq` f x' a'
   else a

--lenA :: A -> Int -> Int
--lenA 

f :: Int -> A -> A -> A
f !x !a !b =
  if x `gtInt` zero then
    g (x `minusInt` one) (A1 b) (A2 a)
   else a

g :: Int -> A -> A -> A
g !x !a !b =
  if x `gtInt` zero then
    g (x `minusInt` one) (A1 b) (A2 a)
   else b
-}

sum' :: Int -> Int
sum' i = go i (I# 0#)
 where
   go :: Int -> Int -> Int
   go !y !res =
     if y `gtInt` zero then
       go (y `minusInt` one) ((res `plusInt` y) `minusInt` I# 100#)
     else
       res

sum'' :: Int -> Int
sum'' (I# y) = go3 y 0#

go3 :: Int# -> Int# -> Int
go3 y res =
  case y ># 0# of
    False -> I# y
    True ->
      go3 (y -# 1#) ((res +# y) -# 100#)

--    go1 y res =
--      if y `gtInt` zero then
--        go (y `minusInt` one) ((res `plusInt` y) `minusInt` I# 100#)
--       else
--         res

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
