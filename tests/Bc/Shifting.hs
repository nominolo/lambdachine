{-# LANGUAGE NoImplicitPrelude, MagicHash, BangPatterns #-}
module Bc.Shifting where

import GHC.Prim
import GHC.Base

up :: Word# -> Int# -> Word#
up w 0# = w
up w n =
  let !w2 = w `uncheckedShiftL#` 1# in
  up w2 (n -# 1#)

dn :: Word# -> Int# -> Word#
dn w 0# = w
dn w n =
  let !w2 = w `uncheckedShiftRL#` 1# in
  dn w2 (n -# 1#)

upv :: Word# -> Int# -> Int# -> Word#
upv w 0# _ = w
upv w n s =
  let !w2 = w `uncheckedShiftL#` s in
  upv w2 (n -# 1#) s

dnv :: Word# -> Int# -> Int# -> Word#
dnv w 0# _ = w
dnv w n s =
  let !w2 = w `uncheckedShiftRL#` s in
  dnv w2 (n -# 1#) s

test = case (((2## `up` 27#) `dn` 25#) `up` 26#) `dn` 28# of
         2## ->
           case (dnv (upv (dnv (upv (dnv (upv 3## 27# 1#) 25# 1#) 14# 2#) 15# 2#)
                          30# 1#) 28# 1#) of
             w -> isTrue# (word2Int# w ==# 12#)
         _ -> False

