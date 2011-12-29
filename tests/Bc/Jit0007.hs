{-# LANGUAGE NoImplicitPrelude, BangPatterns, MagicHash #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Bool.True`con_info
module Bc.Jit0007 where

import GHC.Base
import GHC.Integer
import GHC.Num

--import Debug.Trace
--import Prelude (show)

-- Tests non-constant stack traces

loop :: Int -> Int
loop (I# 0#) = (I# 0#)
loop (I# n#) = 
  let !n' = (I# (n# -# 1#)) in
  let !(I# n'') = loop n' in
  I# (n# +# n'')

test = loop2 15 == 120

loop2 :: Int -> Int
loop2 0 = 0
loop2 n = n + loop2 (n - 1)

{-
    {loop2 5}
  = 5 + {loop {5 - 1}}         -- "+" forces thunk, which tailcalls "loop"
  = 5 + case {5 - 1} of n -> n + {loop {n - 1}}   -- match forces (5 - 1)
  = 5 + case 4 of n -> n + {loop {n - 1}}
  = 5 + (4 + {loop {4 - 1}})

-}