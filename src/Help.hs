{- Handy Calc
Copyright (C) 2016, 2017 Christophe Delord
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

{- The module Help defines help and license messages
-}

{-# LANGUAGE  QuasiQuotes #-}

module Help(license, shortHelp, longHelp, defaultIni) where

import qualified Version as V

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Here.Interpolated

license :: String
license = [iTrim|
${V.name}
(C) ${intercalate ", " (map show V.dates)} Christophe Delord
${V.url}

${V.name} is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

${V.name} is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with ${V.name}.  If not, see <http://www.gnu.org/licenses/>.

${V.name} is powered by Haskell.
|]

shortHelp :: String
shortHelp = [iTrim|
+---------------------------------------------------------------------+
| ${center 31 title} | ${center 15 version} | ${center 15 url} |
|---------------------------------------------------------------------|
| Modes:                          | Numbers:                          |
|     hex oct bin float reset     |     binary: 0b...                 |
|     hex8/16/32/64 ...           |     octal : 0o...                 |
|---------------------------------|     hexa  : 0x...                 |
| Variables and functions:        |     float : 1.2e-3                |
|     variable = expression       |                                   |
|     function(x, y) = expression | Strings   : "abcd"                |
| Multiple statements:            |                                   |
|     expr1; ...; exprn           | Booleans  : true or false         |
|---------------------------------|-----------------------------------|
| Builtin functions:              | Operators:                        |
|     see help                    |     or xor and not                |
|---------------------------------|     < <= > >= == !=               |
| Commands: help license bye      |     cond?expr:expr                |
|           ascii ...             |     + - * / % ** | ^ & >> << ~    |
+---------------------------------------------------------------------+
|]
    where
        title = map toUpper $ intersperse ' ' V.name
        version = "v " ++ intercalate "." (map show V.version)
        url = fromMaybe V.url $ stripPrefix "http://" V.url
        spaces = repeat ' '
        center w s = take w $ take ((w - length s) `div` 2) spaces ++ s ++ spaces

longHelp :: String
longHelp = [iTrim|
Constants                   Value
=========================== ===============================================

nan                         Not a Number
inf                         Infinite
pi                          ${show (pi :: Float)}
e                           ${show (exp 1 :: Float)}

Operators / functions       Description
=========================== ===============================================

+x, -x
x + y, x - y                sum, difference
x * y, x / y                product, division
x // y, x % y               integral division, modulo
x ** y                      x to the power y

~x                          bitwise not
x | y, x ^ y, x & y         bitwise or, xor, and
x << n, x >> n              x left or right shifted by n bits

not x                       boolean not
x or y, x xor y, x and y    boolean or, xor, and
x < y, x <= y               comparisons
x > y, x >= y
x == y, x != y

int(x)                      x converted to int
float(x)                    x converted to float
rat(x)                      x converted to rat

abs(x)                      absolute value of x
ceil(x)                     smallest integer larger than or equal to x
floor(x)                    largest integer smaller than or equal to x
round(x)                    round to the nearest integer
trunc(x)                    tround toward zero
mantissa(x)                 m such that x = m2e, |m| is in [0.5, 1[
exponent(x)                 e such that x = m2e, e is an integer
int(x)                      integral part of x
fract(x)                    fractional part of x
min(x, y), max(x, y)        minimum / maximum value among its arguments

sqr(x)                      square of x (x**2)
sqrt(x)                     square root of x (x**0.5)
cbrt(x)                     cubic root of x (x**(1/3))

cos(x), acos(x), cosh(x)    trigonometric functions
sin(x), asin(x), sinh(x)
tan(x), atan(x), tanh(x)
atan(y, x), atan2(y, x)     arc tangent of y/x (in radians)
deg(x)                      angle x (given in radians) in degrees
rad(x)                      angle x (given in degrees) in radians

exp(x)                      e**x
log(x), ln(x)               logarithm of x in base e
log10(x), log2(x)           logarithm of x in base 10, 2
log(b, x)                   logarithm of x in base b

float2ieee(x)               IEEE 754 representation of x (32 bits)
ieee2float(n)               32 bit float value of the IEEE 754 integer n
double2ieee(x)              IEEE 754 representation of x (64 bits)
ieee2double(n)              64 bit float value of the IEEE 754 integer n

isfinite(x)                 true if x is finite
isinf(x)                    true if x is infinite
isnan(x)                    true if x is not a number

Display modes
=============

dec, hex, oct, bin and str commands change the display mode.
When enabled, the integer result is displayed in
hexadecimal, octal or binary.
float mode shows float values and their IEEE encoding.

dec, hex, oct, bin can have suffixes giving the number of bits
to be displayed (e.g. hex16 shows 16 bit results). Valid suffixes
are 8, 16, 32 and 64.

float can have suffixes giving the size of floats (32 or 64).

The reset command resets the display mode.

Blocks
======

A block is made of several expressions separated by `;`.
The value of the block is the value of the last expression.

e.g. x=1; y=2; x+y defines x=1, y=2 and returns 3

Definitions made in functions are local.

e.g. f(x) = (y=1; x+y) defines a function f that
returns x+1. y is local to f.

Local definitions can be functions.

e.g. fact(n) = (f(n,p)=(n==1)?p:f(n-1,n*p); f(n,1))

Operator precedence
===================

From highest to lowest precedence:

Operator family             Syntax
=========================== =================
Precedence overloading      (...)
Function evaluation         f(...)
Exponentiation              x**y
Unary operators             +x, -y, ~z
Multiplicative operators    * / % & << >>
Additive operators          + - | ^
Relational operators        < <= > >= == !=
Logical not                 not x
Logical and                 and
Logical or                  or xor
Ternary operator            x ? y : z
Assignment                  x = y
Blocks                      expr1; ...; exprn

Other commands              Description
=========================== ===========================

bye, exit, quit             quit
ascii                       print an ASCII table
help                        print this help
version                     print the version number

Credits
=======

${V.tag}
(C) ${intercalate ", " (map show V.dates)} Christophe Delord
${V.url}

|]

defaultIni :: String
defaultIni = [iTrim|
${(unlines . map ("# "++) . init . init . lines) license}
# This file is an example of configuration file for Handy Calc

# Factorial
fact(n) = n < 0 ? nan :
          n == 0 ? 1 :
          n * fact(n-1);

# Fast fibonacci function
fib(n, a, b) = n <= 0 ? a : fib(n-1, b, a+b);
fib(n) = fib(n, 1, 1);

# reverse bit order in a 8 bit ARINC 429 label
label(n) = (
    rev2(b) = ((b&0b10) >> 1) | ((b&0b01) << 1);
    rev4(q) = rev2((q&0b1100) >> 2) | (rev2(q&0b0011) << 2);
    rev8(w) = rev4((w&0xF0) >> 4) | (rev4(w&0x0F) << 4);
    oct; rev8(n);
);
|]
