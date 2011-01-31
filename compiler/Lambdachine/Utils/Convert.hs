module Lambdachine.Utils.Convert where

import Data.Array.ST
import Control.Monad.ST
import Data.Word

-- * Converting floating-point literals to integrals for serialisation

castFloatToWord8Array :: STUArray s Int Float -> ST s (STUArray s Int Word8)
castFloatToWord8Array = castSTUArray

castDoubleToWord8Array :: STUArray s Int Double -> ST s (STUArray s Int Word8)
castDoubleToWord8Array = castSTUArray

castFloatToWord32Array :: STUArray s Int Float -> ST s (STUArray s Int Word32)
castFloatToWord32Array = castSTUArray

castDoubleToWord32Array :: STUArray s Int Double -> ST s (STUArray s Int Word32)
castDoubleToWord32Array = castSTUArray

-- Note: This returns the result in host byte order.

floatToWord32 :: Float -> Word32
floatToWord32 f = runST $ do
  arr <- newArray_ ((0::Int),0)
  writeArray arr 0 f
  arr <- castFloatToWord32Array arr
  readArray arr 0

doubleToWord32s :: Double -> (Word32, Word32)
doubleToWord32s d = runST $ do
  arr <- newArray_ (0::Int, 1)
  writeArray arr 0 d
  arr <- castDoubleToWord32Array arr
  lo <- readArray arr 0
  hi <- readArray arr 1
  return (lo, hi)

floatToBytes :: Float -> [Word8]
floatToBytes f = runST $ do
  arr <- newArray_ ((0::Int),3)
  writeArray arr 0 f
  arr <- castFloatToWord8Array arr
  mapM (readArray arr) [0..3]

doubleToBytes :: Double -> [Word8]
doubleToBytes d = runST $ do
  arr <- newArray_ ((0::Int),7)
  writeArray arr 0 d
  arr <- castDoubleToWord8Array arr
  mapM (readArray arr) [0..7]
