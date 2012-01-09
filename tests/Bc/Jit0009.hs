{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0009 where

import GHC.Prim
import GHC.Types
import GHC.Base

-- A trace that forces the register allocator to spill.

-- tail recursive
loop :: Int# -> Int# -> Int
loop 0# x = I# x
loop n x =
  let !a = x +# 23#
      !b = a *# 2#
      !c = b +# 123#
      !d = c +# 75#
      !e = d +# 74#
      !f = e +# 73#
      !g = f +# 72#
      !h = g +# 71#
      !i = (c *# 2#) +# 74#
      !j = (e *# 2#) +# 73#
      !k = (f *# 2#) +# 72#
      !l = (g *# 2#) +# 71#
      !m = (h *# 2#) +# 70#
      !o = (k *# 2#) +# 70#
      !p = (e *# 2#) +# 70#
  in loop (n -# 1#)
          ((((a *# b) *# (c *# d)) *# ((e *# f) *# (g *# h))) *#
           (((i *# j) *# (j *# k)) *# ((l *# m) *# (o *# p))))

test1 = case loop 10# 1# of
         I# res -> res ==# 6875913138241619968#

-- not tail recursive
loop2 :: Int# -> Int# -> Int
-- loop2 n x | trace (show (I# n, I# x)) False = undefined
loop2 0# x = I# x
loop2 n x =
  let !a = x +# 23#
      !b = a *# 2#
      !c = b +# 123#
      !d = c +# 71#
      !e = d +# 74#
      !f = e +# 73#
      !g = f +# 72#
      !h = g +# 71#
      !i = (c *# 2#) +# 74#
      !j = (e *# 2#) +# 73#
      !k = (f *# 2#) +# 72#
      !l = (g *# 2#) +# 71#
      !m = (h *# 2#) +# 70#
      !o = (k *# 2#) +# 69#
  in
    case loop2 (n -# 1#)
             ((((a *# b) *# (c *# d)) *# ((e *# f) *# (g *# h))) -#
              (((i *# j) *# (j *# k)) *# ((l *# m) *# o)) +# 3#) of
      I# p ->
        I# ((((a *# b) *# (c *# d +# 1#)) *# ((e *# f) *# (g *# h))) *#
                (((i *# j) *# (j *# k)) -# ((l *# m) *# (o *# p))))

test2 = case loop2 10# 2# of
          I# res -> res ==# 1129240707821896384#

test = test1 && test2

