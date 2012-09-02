{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Side0003 where

-- Tests side traces with lots of merged live variables.

import GHC.Prim
import GHC.Types
--import GHC.Base
--import Debug.Trace
--import Prelude

{-# NOINLINE g #-}
g :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
--g a b c d e f | trace (show (I# a, I# b)) False = 0#
g a b c d e f x =
  let !n = if b ># 50# then 5# else 9# in
  a +# b +# c +# d +# e +# f +# n +# x

h :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
h a b0 c0 d0 e0 f0 y0 =
  let !b = b0 -# 4# in
  let !c = c0 *# 2# in
  let !d = d0 -# 6# in
  let !e = e0 +# 7# in
  let !f = f0 *# 3# in
  let !y = y0 -# 1# in
  let !x = g (b +# 1#) (a +# 5#) (c +# 7#) (d +# 9#) (e +# 43#) (f +# 50#) (y *# 2#) in
  if a <=# 0# then a +# b +# c +# d +# e +# f +# x else
    h (a -# 1#) (b +# 1#) d e f y c

test = case h 100# 3# 1# 2# 3# 4# 0# of
--         n -> I# n
         n -> n ==# 89210265937536150#
