{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info 
module Bench.Primes where

import GHC.Prim
import GHC.Bool
import GHC.Types
import GHC.Base
import GHC.Num
import GHC.List

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
  root 30 == 127
  --(root 1500 == 12569)

{-
Example Trace:

root 10
primes !! 10
sub primes 10#
sub (map head (iterate the_filter (iterate succ 2))) 10#
sub (map head (let x0 = iterate succ 2 in x0 : iterate the_filter (the_filter x0))) 10#
sub (let x0 = iterate succ 2 in head x0 : map head (iterate the_filter (the_filter x0))) 10#
let x0 = iterate succ 2 in sub (map head (iterate the_filter (the_filter x0))) 9#
let x0 = ... in sub (map head (let x1 = the_filter x0 in x1 : iterate the_filter (the_filter x1))) 9#
... sub (let x1 = the_filter x0 in head x1 : map head (iterate the_filter (the_filter x1))) 9#
let x0 = ...; x1 = ... in sub (map head (iterate the_filter (the_filter x1))) 8#
...
let x0 = ...; ...; x9 = ... in sub (map head (iterate the_filter (the_filter x9))) 0#
... sub (map head (let x10 = the_filter x9 in x10 : iterate the_filter (the_filter x10))) 0#
... sub (let x10 = ... in head x10 : map head (iterate ...)) 0#
... let x10 = the_filter x9 in head x10

-- the_filter is strict in its argument, so x10 forces x9, x9 forces x8, etc.
let x0 = 2 : iterate succ (succ 2); let x1 = the_filter x0; ... in head x10
... x1 = filter (isdivs 2) (iterate succ (succ 2)) ...
... x1 = 2 : filter (isdivs 2) (iterate succ (succ 2)) ...

-}
