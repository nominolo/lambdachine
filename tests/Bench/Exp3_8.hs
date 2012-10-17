{-# LANGUAGE NoImplicitPrelude, CPP #-}
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.Exp3_8 where
#endif

import GHC.Base
import GHC.Num

infix 8 ^^^

data Nat = Z | S Nat deriving (Eq,Ord)

plusNat :: Nat -> Nat -> Nat
plusNat Z     y = y
plusNat (S x) y = S (x `plusNat` y)

timesNat :: Nat -> Nat -> Nat
timesNat x Z = Z
timesNat x (S y) = (x `timesNat` y) `plusNat` x

fromInt :: Int -> Nat
fromInt x = if x < 1 then Z else S (fromInt (x-1))

-- partain:sig
int :: Nat -> Int

int Z     = 0
int (S x) = 1 + int x

x ^^^ Z   = S Z
x ^^^ S y = x `timesNat` (x ^^^ y)

run power = int (fromInt 3 ^^^ fromInt power)

-- -------------------------------------------------------------------

test = run 3 == 27
-- test = run 5 == 243
--bench = run 8 == 6561  -- too short
bench = run 9 == 19683

#ifdef BENCH_GHC
main = print bench
#endif
