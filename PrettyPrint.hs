module PrettyPrint where

import Expression

import Data.Char
import Data.List
import Data.Ratio

import IEEE754

prompt :: String -> String
prompt ":" = ": "
prompt msg = msg ++ replicate (8 - length msg) ' '

showBase :: Int -> Integer -> Int -> String
showBase b x s | x < 0 && s == 0 = "-" ++ showBase b (negate x) s
showBase b x s = prefix ++ fmt (padding ++ map toUpper ds)
    where
        prefix | b == 16 = "0x"
               | b ==  8 = "0o"
               | b ==  2 = "0b"
               | otherwise = ""
        ds = toBase x
        padding | b == 0 = ""
                | otherwise = replicate (nbDigits - length ds) '0'
        nbDigits = length (toBase (2^s-1))
        toBase 0 = ""
        toBase n = toBase n' ++ [intToDigit (fromInteger d)]
            where (n', d) = n `divMod` fromIntegral b
        fmt "" = "0"
        fmt xs = xs

pp :: Conf -> Expr -> String
pp cfg (Z x) = intercalate "\n" xs
    where
        xs = show x : ppBase dec "dec" 10 ++ ppBase hex "hex" 16 ++ ppBase oct "oct" 8 ++ ppBase bin "bin" 2
        x' = x `mod` (2^size cfg)
        ppBase f str b = case (f cfg, size cfg) of
            (False, _) -> []
            (True, 0) -> [prompt str ++ showBase b x 0]
            (True, n) -> [prompt (str++show n) ++ showBase b x' n]

pp cfg (Q x) = intercalate "\n" xs
    where
        xs = (show n ++ "/" ++ show d) : ppFloat float
        n = numerator x
        d = denominator x
        ppFloat f = case (f cfg, size cfg) of
            (True, 32) -> [prompt "~float32" ++ show xf32 ++ " <=> " ++ showBase 16 xi32 32]
            (True, 64) -> [prompt "~float64" ++ show xf64 ++ " <=> " ++ showBase 16 xi64 64]
            (_, _) -> [prompt "~" ++ show (fromRational x :: Double)]
        xf32 = doubleToFloat (fromRational x)
        xi32 = floatToInteger xf32
        xf64 = fromRational x
        xi64 = doubleToInteger xf64

pp cfg (R x) = intercalate "\n" xs
    where
        xs = show x : ppFloat float
        ppFloat f = case (f cfg, size cfg) of
            (True, 32) -> [prompt "float32" ++ show xf32 ++ " <=> " ++ showBase 16 xi32 32]
            (True, 64) -> [prompt "float64" ++ show xf64 ++ " <=> " ++ showBase 16 xi64 64]
            _ -> []
        xf32 = doubleToFloat x
        xi32 = floatToInteger xf32
        xf64 = x
        xi64 = doubleToInteger xf64

pp _ (B True) = "true"
pp _ (B False) = "false"
pp _ (S s) = show s

pp _ (Put _ s) = s

pp _ Nop = ""

pp _ (E err) = err
pp _ x = "Cannot display the value of " ++ show x
