{- Handy Calc
Copyright (C) 2016, 2017, 2018 Christophe Delord
https://cdsoft.fr/hcalc

This file is part of Handy Calc.

Handy Calc is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Handy Calc is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Handy Calc.  If not, see <http://www.gnu.org/licenses/>.
-}

{- The module IEEE754 converts floating point numbers
from/to their IEEE754 representation.
It is coded in C and uses simple casts to make the conversion.
This module is the Haskell interface to the C implementation.
-}

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
