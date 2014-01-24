{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples, BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
#ifdef BENCH_GHC
import Prelude ( print )
import GHC.Prim
import GHC.Types (Int(..))
#else
module Bc.Basic0005 where

import GHC.Prim

data Int = I# Int#

#endif

data List a = Nil | Cons a (List a)

-- {- # NOINLINE plusInt #-}
-- plusInt :: Int -> Int -> Int
-- plusInt (I# x) (I# y) = I# (x +# y)

upto :: Int -> Int -> List Int
upto lo@(I# lo') hi@(I# hi') =
  if lo' ># hi' then Nil
  else
    let !lo2 = I# (lo' +# 1#) in
    Cons lo (upto lo2 hi)

sum ls = go 0# ls
 where
   go acc Nil = I# acc
   go acc (Cons (I# n) xs) = go (acc +# n) xs

zero, one :: Int
zero = I# 0#
one = I# 1#

-- test :: Int
-- test = nth 300# (rep one)

more = I# 200000000#

{-# NOINLINE root #-}
root n = sum (upto one n)

test = root more

#ifdef BENCH_GHC
main = print test
#endif



-- {-# NOINLINE rep #-}
-- rep :: Int -> List Int
-- rep n =
--   let x _ = Cons n (x ()) in x ()

-- nth :: Int# -> List Int -> Int
-- nth n l = case l of
--   Nil -> zero
--   Cons x xs ->
    -- if n ==# 0# then x else nth (n -# 1#) xs

-- cons :: a -> List a -> List a
-- cons x y = Cons x y

