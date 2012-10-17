{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash, CPP #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info 
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.Append where
#endif

import GHC.Types
import GHC.Integer
import GHC.Num
import GHC.Base

{- # NOINLINE replicate #-}
replicate :: Int -> a -> [a]
replicate n x =
  if n == 0 then [] else x : replicate (n - 1) x

{- # NOINLINE length #-}
length :: [a] -> Int
length l = len l 0#
  where
    len []     a# = I# a#
    len (_:xs) a# = len xs (a# +# 1#)

append3 xs ys zs = (xs ++ ys) ++ zs

root n = length (append3 (replicate n 1) (replicate n 2) (replicate n 3))
           == 3 * n

test = root 200 -- max: 1820

bench = root 30000000

#ifdef BENCH_GHC
main = print bench
#endif
