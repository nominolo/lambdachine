{-# LANGUAGE CPP, NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.I#`con_info 3
#ifdef BENCH_GHC
import Prelude ( print )
#else
module Bench.Tak where
#endif

import GHC.Types
import GHC.Base
import GHC.Num

tak :: Int -> Int -> Int -> Int
tak x y z = if not (y `ltInt` x)
            then z
            else tak (tak (x `minusInt` 1) y z)
                     (tak (y `minusInt` 1) z x)
                     (tak (z `minusInt` 1) x y)

{-# NOINLINE root #-}
root x y z = tak x y z

#ifdef BENCH_GHC
main = print bench
#endif

bench = root 33 17 8

test = root 8 4 2
  
-- root 24 16 8
--  


