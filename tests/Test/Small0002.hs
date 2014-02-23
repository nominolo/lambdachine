{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.True`con_info
module Test.Small0002 where

import GHC.Prim
import GHC.Types
import GHC.Base ( Monad(..) )
import GHC.ST

{-# NOINLINE g #-}
g :: Int -> ST s Bool
g (I# n) = return (isTrue# (n ==# 6#))

test = runST (g 6)
