{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash, CPP #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.SumFromTo1 where
#endif

import GHC.Prim
import GHC.Types

#ifdef USE_NOINLINE
{-# NOINLINE enumFromTo #-}
{-# NOINLINE sum_aux #-}
{-# NOINLINE root #-}
#endif

enumFromTo :: Int -> Int -> [Int]
enumFromTo from@(I# m) to@(I# n) =
  if isTrue# (m ># n) then [] else
    from : enumFromTo (I# (m +# 1#)) to

zero :: Int
zero = I# 0#

one :: Int
one = I# 1#

sum :: [Int] -> Int
sum l = sum_aux (I# 0#) l

sum_aux :: Int -> [Int] -> Int
sum_aux !acc [] = acc
sum_aux !(I# a) (I# x:xs) = sum_aux (I# (a +# x)) xs

timesInt :: Int -> Int -> Int
timesInt (I# m) (I# n) = I# (m *# n)

plusInt :: Int -> Int -> Int
plusInt (I# m) (I# n) = I# (m +# n)

succInt :: Int -> Int
succInt (I# m) = I# (m +# 1#)

eqInt :: Int -> Int -> Bool
eqInt (I# m) (I# n) = isTrue# (m ==# n)

root upper =
  let !l = sum (enumFromTo one upper) in
  (l `plusInt` l) `eqInt` (upper `timesInt` (succInt upper))

test = root (I# 100#)

bench = root (I# 200000000#)

#ifdef BENCH_GHC
main = print bench
#endif
