{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc
module Bc.Bc0001 where

import GHC.Prim
import GHC.Types ( Bool(..) )

data Int = I# Int#

data List a = Nil | Cons a (List a)

f x = x

g f x = f x

h y = Nil

i (I# n) = n

hd (Cons x _) = x
hd Nil = I# 3#

a x y = case x ># y of
  True -> 1#
  False -> 2#

b x y = x ># y

c x = case x of
  3# -> 4#
  4# -> 5#
  7# -> 3#
  9# -> 8#
  n -> n

d x = case e x of
  (# y, z #) -> y +# z

{-# NOINLINE e #-}
e :: Int# -> (# Int#, Int# #)
e x = (# x, x #)

{-
f :: Int# -> Int#
f x = x

g :: Int# -> Int#
g x = 1#

h :: Int# -> Int#
h y = y +# 2#

i :: Int# -> Int#
i y = y +# y

j :: Int# -> Int# -> Int#
j x y = x +# (y *# 3#)

c :: Int# -> Int
c x = I# 3#

d :: a -> ()
d x = ()
-}
-- {-# NOINLINE plusInt #-}
-- plusInt :: Int -> Int -> Int
-- plusInt (I# m) (I# n) = I# (m +# n)

-- gtInt :: Int -> Int -> Bool
-- gtInt (I# m) (I# n) = m ># n

-- z :: Int -> Int
-- z x = plusInt x (plusInt x x)

--sum :: 


-- i42 = I# 42#
{-
l1 = Cons i42 l2
l2 = Cons (I# 23#) l1
-}
