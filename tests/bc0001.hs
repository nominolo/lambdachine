{-# LANGUAGE NoImplicitPrelude, MagicHash #-}
module Bc0001 where

import GHC.Prim
import GHC.Bool

data Int = I# Int#

data List a = Nil | Cons a (List a)

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

{-# NOINLINE plusInt #-}
plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)

gtInt :: Int -> Int -> Bool
gtInt (I# m) (I# n) = m ># n

z :: Int -> Int
z x = plusInt x (I# 42#)

i42 = I# 42#

l1 = Cons i42 l2
l2 = Cons (I# 23#) l1
