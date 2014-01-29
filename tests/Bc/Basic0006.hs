{-# LANGUAGE NoImplicitPrelude, MagicHash, UnboxedTuples #-}
module Bc.Basic0006 where

import GHC.Prim
import GHC.Types

-- Test code gen for function calls with very many arguments:

{-# NOINLINE f #-}
f :: Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
  -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
  -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int# -> Int#
  -> Int#
f x01 x02 x03 x04 x05 x06 x07 x08 x09 x10
  x11 x12 x13 x14 x15 x16 x17 x18 x19 x20
  x21 x22 x23 x24 x25 x26 x27 x28 x29 x30 =
   (x01 +# x02 +# x03 +# x04 +#x05 +# x06 +#x07 +# x08 +#x09 +# x10) -#
   (x11 +# x12 +# x13 +# x14 +#x15 +# x16 +#x17 +# x18 +#x19 +# x20) -#
   (x21 +# x22 +# x23 +# x24 +#x25 +# x26 +#x27 +# x28 +#x29 +# x30)

g :: Int# -> Int#
g x =
  f 1# 2# 3# 4# 5# 1# 2# 3# 4# 5#
        1# 2# 3# 4# 5# 1# 2# 3# 4# 5#
        1# 2# 3# 4# 5# 1# 2# 3# 4# 5# ==# x

test = isTrue# (g -30#)
