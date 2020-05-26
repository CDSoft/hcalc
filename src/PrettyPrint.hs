{- Handy Calc
Copyright (C) 2016-2020 Christophe Delord
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

{- The module PrettyPrint converts expressions to strings.
The configuration is used to display the value in various formats.
-}

module PrettyPrint(prompt, pp) where

import Expression
import IEEE754

import Data.Char
import Data.List
import Data.Ratio
import Numeric

-- prompt makes a string that can be used as a prompt
-- to get user input or to precise the type of a display.
prompt :: String -> String
-- user input
prompt ":" = ": "
-- display type (fixed length)
prompt msg = msg ++ replicate (8 - length msg) ' '

-- showBase b x s is a string representing the number x in base b
-- with a size of s digits (no padding if s == 0)
showBase :: Integer -> Integer -> Int -> String
showBase b x 0 | x < 0 = '-' : showBase b (negate x) 0
showBase b x s = prefix ++ pad (showIntAtBase b intToDigit x "")
    where
        prefix | b == 16   = "0x"
               | b == 8    = "0o"
               | b == 2    = "0b"
               | otherwise = ""
        pad n | s == 0 = n
        pad n = replicate (maxLen - length n) '0' ++ n
            where
                maxLen = (ceiling :: Double -> Int) $ logBase (fromIntegral b) (2^s-1)

-- pp converts a value to a string, according to the current dispay mode
pp :: Conf -> Expr -> String
pp cfg (Z x) = prompt "=" ++ intercalate "\n" xs
    where
        xs = show x :  ppBase dec "dec" 10
                    ++ ppBase hex "hex" 16
                    ++ ppBase oct "oct" 8
                    ++ ppBase bin "bin" 2
                    ++ ppFloat float
        x' = x `mod` (2^size cfg)
        ppBase f str b = case (f cfg, size cfg) of
            (False, _) -> []
            (True, 0) -> [prompt str ++ showBase b x 0]
            (True, n) -> [prompt (str++show n) ++ showBase b x' n]
        ppFloat f = case (f cfg, size cfg) of
            (True, 32) -> [prompt "flt32" ++ show xf32 ++ " <=> " ++ showBase 16 xi32 32]
            (True, 64) -> [prompt "flt64" ++ show xf64 ++ " <=> " ++ showBase 16 xi64 64]
            _ -> []
        xf32 = integerToFloat x
        xf64 = integerToDouble x
        xi32 = word32ToInteger $ integerToWord32 x
        xi64 = word64ToInteger $ integerToWord64 x

pp cfg (Q x) = prompt "=" ++ intercalate "\n" xs
    where
        xs = (show n ++ "/" ++ show d) : ppFloat float
        n = numerator x
        d = denominator x
        ppFloat f = case (f cfg, size cfg) of
            (True, 32) -> [prompt "~flt32" ++ show xf32 ++ " <=> " ++ showBase 16 xi32 32]
            (True, 64) -> [prompt "~flt64" ++ show xf64 ++ " <=> " ++ showBase 16 xi64 64]
            (_, _) -> [prompt "~" ++ show (fromRational x :: Double)]
        xf32 = doubleToFloat (fromRational x)
        xi32 = floatToInteger xf32
        xf64 = fromRational x
        xi64 = doubleToInteger xf64

pp cfg (R x) = prompt "=" ++ intercalate "\n" xs
    where
        xs = show x : ppFloat float
        ppFloat f = case (f cfg, size cfg) of
            (True, 32) -> [prompt "flt32" ++ show xf32 ++ " <=> " ++ showBase 16 xi32 32]
            (True, 64) -> [prompt "flt64" ++ show xf64 ++ " <=> " ++ showBase 16 xi64 64]
            _ -> []
        xf32 = doubleToFloat x
        xi32 = floatToInteger xf32
        xf64 = x
        xi64 = doubleToInteger xf64

pp _ (B True) = prompt "=" ++ "true"
pp _ (B False) = prompt "=" ++ "false"
pp _ (S s) = prompt "=" ++ show s

pp _ (Put _ s) = s

pp _ None = ""

pp _ (E err) = prompt "!" ++ err
pp _ x = prompt "!" ++ "Cannot display the value of <" ++ show x ++ ">"
