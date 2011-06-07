module Lambdachine.Utils.IO where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as U
import Blaze.ByteString.Builder
import Blaze.ByteString.Builder.ByteString
import Blaze.ByteString.Builder.Word
import qualified Blaze.ByteString.Builder.Char8 as B
import Data.Monoid
import Data.Word
import Data.Bits

varUIntBytes :: Word -> [Word8]
varUIntBytes w
  | w < 128 = [w2b w]
  | otherwise =
    (w2b w .|. 0x80) : go (w `shiftR` 7)
 where
   go w | w < 128 = [w2b w]
   go w = (w2b w .|. 0x80) : go (w `shiftR` 7)
{-# INLINE varUIntBytes #-}

varUInt :: Word -> Builder
varUInt w = fromWord8s (varUIntBytes w)
{-# INLINE varUInt #-}

w2b :: Word -> Word8
w2b = fromIntegral
{-# INLINE w2b #-}

i2w :: Int -> Word
i2w = fromIntegral
{-# INLINE i2w #-}

w2i :: Word -> Int
w2i = fromIntegral
{-# INLINE w2i #-}

varSInt :: Int -> Builder
varSInt n = varUInt (zigZagEncode n)

zigZagEncode :: Int -> Word
zigZagEncode n =
  if n < 0
    then (complement (i2w n) `shiftL` 1) + 1
    else i2w n `shiftL` 1

zigZagDecode :: Word -> Int
zigZagDecode w =
  if testBit w 0
    then w2i (complement (w `shiftR` 1))
    else w2i (w `shiftR` 1)

encodeString :: String -> Builder
encodeString s =
  varUInt (i2w (S.length bytes)) `mappend` fromByteString bytes
 where
   bytes = U.fromString s

test f = do
  L.writeFile f $ toLazyByteString $ mconcat $
    [ B.fromString "KHCB"
    , fromWrite $ mconcat $
        [ writeWord16be 0  -- major
        , writeWord16be 1  -- minor
        , writeWord32be 0  -- flags
        , writeWord32be 3  -- numStrings
        , writeWord32be 0  -- numInfoTables
        , writeWord32be 0  -- numObjects
        ]
    , B.fromString "BCST" -- section magic
    , mconcat [ encodeString "Foo"
              , encodeString "Bar"
              , encodeString "Baz"
              ]
    , mconcat $ map varUInt [3, 0, 2, 1]
        -- = identifier Foo.Baz.Bar
    ]
{-
    ]

-}
--    toLazyByteString (encodeString "FooBar")

--test = L.unpack $ toLazyByteString (fromWrite (writeVarUInt 300))

--writeVarSInt :: Word