module IEEE754 where

import Data.Word

foreign import ccall "doubleToWord64" doubleToWord64 :: Double -> Word64
foreign import ccall "floatToWord32" floatToWord32 :: Float -> Word32

foreign import ccall "word64ToDouble" word64ToDouble :: Word64 -> Double
foreign import ccall "word32ToFloat" word32ToFloat :: Word32 -> Float

foreign import ccall "floatToDouble" floatToDouble :: Float -> Double
foreign import ccall "doubleToFloat" doubleToFloat :: Double -> Float

word32ToInteger :: Word32 -> Integer
word32ToInteger = fromIntegral

integerToWord32 :: Integer -> Word32
integerToWord32 = fromIntegral

word64ToInteger :: Word64 -> Integer
word64ToInteger = fromIntegral

integerToWord64 :: Integer -> Word64
integerToWord64 = fromIntegral

doubleToInteger :: Double -> Integer
doubleToInteger = word64ToInteger . doubleToWord64

floatToInteger :: Float -> Integer
floatToInteger = word32ToInteger . floatToWord32

integerToDouble :: Integer -> Double
integerToDouble = word64ToDouble . integerToWord64

integerToFloat :: Integer -> Float
integerToFloat = word32ToFloat . integerToWord32
