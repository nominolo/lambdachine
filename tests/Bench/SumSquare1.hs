{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash, CPP #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.SumSquare1 where
#endif

import GHC.Prim
import GHC.List
import GHC.Base
import GHC.Num

#ifdef USE_NOINLINE
{-# NOINLINE enumFromTo'Int #-}
{-# NOINLINE sum_aux #-}
{-# NOINLINE root #-}
#endif

enumFromTo'Int :: Int -> Int -> [Int]
enumFromTo'Int from@(I# m) to@(I# n) =
  if m ># n then [] else
    from : enumFromTo'Int (I# (m +# 1#)) to

sum :: [Int] -> Int
sum l = sum_aux (I# 0#) l

{-# NOINLINE sum_aux #-}
sum_aux :: Int -> [Int] -> Int
sum_aux !acc [] = acc
sum_aux !(I# a) (I# x:xs) = sum_aux (I# (a +# x)) xs

{-# NOINLINE root #-}
root :: Int -> Int
root x = sum [ I# (a# *# b#) 
             | a@(I# a#) <- enumFromTo'Int 1 x
             , I# b# <- enumFromTo'Int a x ] 

test = root 20 == 23485

bench = root 15000

#ifdef BENCH_GHC
main = print bench
#endif
