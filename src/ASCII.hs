{- Handy Calc
Copyright (C) 2016, 2017, 2018 Christophe Delord
http://cdsoft.fr/hcalc

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

{- The module ASCII defines an ASCII table
-}

module ASCII(ascii) where

import Data.Char

-- `ascii` contains the whole 8 bit ASCII table
ascii :: String
ascii = unlines $ [ sep1
                  , hdr1
                  , sep1
                  ] ++
                  [ line1 i (i+32) | i <- [0..31] ] ++
                  [ sep2 ] ++
                  [ line2 i (i+16) (i+32) (i+48) | i <- [64..79] ] ++
                  [ sep3 ] ++
                  [ line2 i (i+16) (i+32) (i+48) | i <- [128..143] ] ++
                  [ sep3 ] ++
                  [ line2 i (i+16) (i+32) (i+48) | i <- [192..207] ] ++
                  [ sep4 ]
    where
        sep1 = "+=====+=====+===================================+=====+=====+===========+"
        hdr1 = "| Dec | Hex | Character                         | Dec | Hex | Character |"
        sep2 = "+-----+-----+-----------------------------------+-----+-----+-----------+"
        sep3 = "+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+-----+"
        sep4 = "+=====+=====+=====+=====+=====+=====+=====+=====+=====+=====+=====+=====+"
        names = [ (   0 , "NUL '\\0'")
                , (   1 , "SOH (start of heading)")
                , (   2 , "STX (start of text)")
                , (   3 , "ETX (end of text)")
                , (   4 , "EOT (end of transmission)")
                , (   5 , "ENQ (enquiry)")
                , (   6 , "ACK (acknowledge)")
                , (   7 , "BEL '\\a' (bell)")
                , (   8 , "BS  '\\b' (backspace)")
                , (   9 , "HT  '\\t' (horizontal tab)")
                , (  10 , "LF  '\\n' (new line)")
                , (  11 , "VT  '\\v' (vertical tab)")
                , (  12 , "FF  '\\f' (form feed)")
                , (  13 , "CR  '\\r' (carriage ret)")
                , (  14 , "SO  (shift out)")
                , (  15 , "SI  (shift in)")
                , (  16 , "DLE (data link escape)")
                , (  17 , "DC1 (device control 1)")
                , (  18 , "DC2 (device control 2)")
                , (  19 , "DC3 (device control 3)")
                , (  20 , "DC4 (device control 4)")
                , (  21 , "NAK (negative ack.)")
                , (  22 , "SYN (synchronous idle)")
                , (  23 , "ETB (end of trans. blk)")
                , (  24 , "CAN (cancel)")
                , (  25 , "EM  (end of medium)")
                , (  26 , "SUB (substitute)")
                , (  27 , "ESC (escape)")
                , (  28 , "FS  (file separator)")
                , (  29 , "GS  (group separator)")
                , (  30 , "RS  (record separator)")
                , (  31 , "US  (unit separator)")
                , (  32 , "SPACE")
                ]
        name i w = s ++ replicate (w - length s) ' '
            where s = case lookup i names of
                        Just n -> n
                        Nothing | 128 <= i && i <= 159 -> " "
                                | otherwise            -> [chr i]
        dec i = replicate (3 - length s) ' ' ++ s where s = show i
        hex i = replicate (3 - length s) ' ' ++ s
            where
                s = [digit (i`div`16), digit (i`mod`16)]
                digit d | d <= 9    = chr (ord '0' + d)
                        | otherwise = chr (ord 'A' + d - 10)
        line1 i j = "| " ++ dec i ++ " | " ++ hex i ++ " | " ++ name i 33 ++ " | "
                         ++ dec j ++ " | " ++ hex j ++ " | " ++ name j  9 ++ " |"
        line2 i j k l = "| " ++ dec i ++ " | " ++ hex i ++ " |  " ++ name i 1 ++ "  | "
                             ++ dec j ++ " | " ++ hex j ++ " |  " ++ name j 1 ++ "  | "
                             ++ dec k ++ " | " ++ hex k ++ " |  " ++ name k 1 ++ "  | "
                             ++ dec l ++ " | " ++ hex l ++ " |  " ++ name l 1 ++ "  |"
