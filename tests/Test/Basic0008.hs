{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc
module Test.Basic0008 where

import GHC.Prim

data Int = I# Int#

data X = A | B X | C X | D

-- The pattern match, combined with the unlifted result type will cause GHC
-- 7.8.* to generate a reference to "void#". The bytecode compiler should treat
-- this correctly as a function that takes no runtime arguments (or a single
-- arbitrary argument which will be ignored by the body).

{-# NOINLINE f #-}
f :: X -> Int#
f A = 3#
f (B (C D)) = 4#

test = I# (f A)

--test = raise# (I# 42#)
