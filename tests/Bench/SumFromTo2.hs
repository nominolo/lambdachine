{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash, CPP #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.SumFromTo2 where
#endif
import GHC.Prim
import GHC.Bool
import GHC.Types
import GHC.Base
import GHC.Num

#ifdef USE_NOINLINE
{-# NOINLINE enumFromTo #-}
{-# NOINLINE root #-}
#endif

enumFromTo :: Int -> Int -> [Int]
enumFromTo from@(I# m) to@(I# n) =
  if m ># n then [] else
    from : enumFromTo (I# (m +# 1#)) to

-- We keep this NOINLINE, because otherwise GHC specializes this
-- code to Ints.  However, we want to measure the overhead of
-- the overloaded call, so specializing to Int would make that
-- measurement useless.

{-# NOINLINE sum #-}
sum :: (Num a) => [a] -> a
sum     l       = sum' l 0
  where
    sum' []     !a = a
    sum' (x:xs) !a =
      let !a' = a + x in sum' xs a'

succInt :: Int -> Int
succInt (I# m) = I# (m +# 1#)

root upper =
  let !l = sum (enumFromTo 1 upper) in
  (l `plusInt` l) `eqInt` (upper `timesInt` (succInt upper))

test = root (I# 100#)

bench = root (I# 200000000#)

#ifdef BENCH_GHC
main = print bench
#endif

