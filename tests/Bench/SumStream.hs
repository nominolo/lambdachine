{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash, CPP,
             ExistentialQuantification #-}
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.SumStream where
#endif
import GHC.Base
import GHC.Num

data Stream a = forall s. Stream !s (s -> Step s a)

data Step s a
  = Yield a s
  | Done
  | Skip s

uptoS :: Int -> Int -> Stream Int
uptoS from to@(I# n) = Stream from next
 where
   next ii@(I# i) =
     if i ># n then Done
               else
                 let i' = I# (i +# 1#) in
                 Yield ii i'

sumS :: Num a => Stream a -> a
sumS (Stream s0 next) = go s0 0
 where
   go s !acc = case next s of
                 Done -> acc
                 Skip s' -> go s' acc
                 Yield a s' ->
                   let !acc' = acc + a in
                   go s' acc'

root upper =
  let !l = sumS (uptoS one upper) in
  (l `plusInt` l) `eqInt` (upper `timesInt` (succInt upper))


one :: Int
one = I# 1#

-- timesInt :: Int -> Int -> Int
-- timesInt (I# m) (I# n) = I# (m *# n)

-- plusInt :: Int -> Int -> Int
-- plusInt (I# m) (I# n) = I# (m +# n)

succInt :: Int -> Int
succInt (I# m) = I# (m +# 1#)

test = root (I# 100#)

bench = root (I# 200000000#)

#ifdef BENCH_GHC
main = print bench
#endif

