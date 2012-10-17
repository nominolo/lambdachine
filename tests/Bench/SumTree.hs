{-# LANGUAGE NoImplicitPrelude, CPP #-}
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.SumTree where
#endif

import GHC.Base
import GHC.Num

data Tree a = Leaf a | Branch (Tree a) (Tree a)

sumtr :: Tree Int -> Int
sumtr t = case t of
    Leaf x -> x
    Branch l r -> sumtr l + sumtr r

squaretr :: Tree Int -> Tree Int
squaretr t = case t of
    Leaf x -> Leaf (x*x)
    Branch l r -> Branch (squaretr l) (squaretr r)

buildTree :: Int -> Tree Int -> Tree Int
buildTree n t = case n == 0 of
    True -> t
    False -> buildTree (n-1) (Branch t t)

root :: Int -> Int
root n = sumtr (squaretr (buildTree n (Leaf 1)))

test = root 5 == 32

bench = root 25 == 33554432 -- == 4194304

#ifdef BENCH_GHC
main = print bench
#endif
