{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples, CPP #-}
-- RUN: %bc_vm_chk
-- CHECK: @Result@ IND -> GHC.Types.True`con_info
#ifndef REALGHC
module Test.Small0003 where
#else
module Main where

import Prelude ( print )
#endif

import GHC.Prim
import GHC.Types
import GHC.Base ( Monad(..) )
import GHC.ST

data MutableByteArray s = MutableByteArray !Int (MutableByteArray# s)

{-# NOINLINE g #-}
g :: Int -> ST s Bool
g (I# n) = do
  b <- ST (\s -> case newByteArray# 5# s of
                   (# s', ba #) -> (# s', MutableByteArray 5 ba #))
  ST (\s -> case b of
              MutableByteArray _ ba# ->
                case writeInt8Array# ba# 3# 42# s of
                  s' -> (# s', () #))

  I# n' <- ST (\s -> case b of
                       MutableByteArray _ ba# ->
                         case readInt8Array# ba# 3# s of
                           (# s', n' #) -> (# s', I# n' #) )


  return (isTrue# (n' ==# 42#))

test = runST (g 6)

#ifdef REALGHC
main = print test
#endif

