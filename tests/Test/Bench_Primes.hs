{-# LANGUAGE CPP, NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.True`con_info
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Test.Bench_Primes where
#endif

import GHC.Prim
import GHC.Types
import GHC.Base
import GHC.List

#ifdef BENCH_GHC
main = print bench
#endif

isdivs :: Int -> Int -> Bool
isdivs n x = modInt x n /= 0

the_filter :: [Int] -> [Int]
the_filter (n:ns) = filter (isdivs n) ns

succ :: Int -> Int
succ (I# n) = I# (n +# 1#)

{-# NOINLINE root #-}
root :: Int -> Int
root n = primes !! n
  where
    primes :: [Int]
    primes = map head (iterate the_filter (iterate succ 2))

test = 
  root 25 == 101

bench =
  root 1500 == 12569

