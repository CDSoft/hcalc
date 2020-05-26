% Handy Calc Manual
% Christophe Delord
% {{mdate}}

<!--
Handy Calc
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
-->

Introduction
============

Handy Calc is a simple yet powerful calculator.
Unlike most of other calculators, Handy Calc is based on a textual interface.
It may seem a bit spartan and outdated but entering expressions with the keyboard
is way easier than with a mouse.
And you get nice editing features for free (edition, copy/paste, history, ...).

Handy Calc is also an application example for the [FUN](https://cdsoft.fr/fun) project.
Its development process and methods are based on:

- Haskell
- static and strong typing
- extreme compiler checks
- executable specification with embedded tests
- unit testing to ease evolutions and non regression checks
- code coverage to measure the completeness of the tests

So Handy Calc is supposed to be better and safer than its predecessor
([Calculadoira](https://cdsoft.fr/calculadoira)).

If you like Handy Calc, please consider supporting my [FUN](https://cdsoft.fr/fun) project.

You can also contribute to [Handy Calc on GitHub](http://github.com/CDSoft/hcalc).

License
=======

~~~~~~~~~~~~~~~~~~ {cmd=bash}
hcalc license
~~~~~~~~~~~~~~~~~~

Download and installation
=========================

[ABP]: https://cdsoft.fr/abp
[Pandoc]: http://pandoc.org/
[Haskell]: https://www.haskell.org/
[The Haskell Tool Stack]: https://docs.haskellstack.org/en/latest/README/

The current version is [{{version}}]({{hcalc_url}})

**Installation from sources on Linux or Windows:**

- Prerequisites
    - [The Haskell Tool Stack]
    - Cygwin or MSYS/Mingw properly installed on Windows
    - [ABP] and [Pandoc] to generate the documentation (optional)
- Download and unpack [{{hcalc_tar}}]({{hcalc_url}})
- Run make
    - `make` compiles `hcalc`
    - `make install` installs `hcalc` in `~/.local/bin`
    - `make doc` generates the documentation in `doc`
    - `make test` runs the non regression tests

**Binaries:**

The binaries are not provided anymore. Use the source Luke!

**Notes:**

For a better user experience on Linux, it is recommended to use Handy Calc
with [`rlwrap`](http://utopia.knoware.nl/~hlub/rlwrap/rlwrap.man.html)
(e.g. `rlwrap hcalc`).
`rlwrap` will give Handy Calc nice editing features.

I use a keyboard shortcut to start Handy Calc in a terminal:

~~~~~ .bash
urxvt +sb -T hCalc -e rlwrap ~/bin/hcalc
~~~~~

Screenshot
==========

~~~~~~~~~~~~~~~~~~ {cmd=bash}
hcalc bye | grep -v ^Loading
~~~~~~~~~~~~~~~~~~

Command line usage
==================

Handy Calc can be used on the command line.
Each argument is considered as an expression to be evaluated.
Only the value of the last expression is printed.

~~~ {.bash cmd=bash}
echo '$ hcalc "x = 21" "y = 2" "x * y"'

hcalc "x = 21" "y = 2" "x * y"
~~~

Interactive usage
=================

The main usage of Handy Calc is by interacting in a terminal.
Expressions are entered with the keyboard, evaluated and the result is printed.
The next section lists all the operators and functions provided by Handy Calc.

A typical interactive session looks like this:

``` meta
hcalc: hcalc < % | sed /Loading/d | tac | sed 1d | tac | sed 1d
```

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = 21
y = 2
(x * y) ** 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

User's manual
=============

``` meta
hcalc: hcalc < % | sed 1,/Loading/d | tac | sed 1d | tac | sed 1d
```

## Numbers

### Integers

Integers can be decimal, hexadecimal, octal or binary numbers:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
42
0x24
0o37
0b1010
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Rational numbers

Rational numbers can be used to make *exact* computations instead of
using floating point numbers.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
1 + 2/3
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some functions don't support rational numbers and will produce floating point numbers.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
1/2 + cos(0)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Floating point numbers

Floating point numbers are single (32 bit) or double (64 bits) precision
floating point numbers.

They are represented internally by 64 bit numbers but can be converted to 32 bit
numbers as well as to their IEEE 754 representation.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
3.14
1.23e-6
e
pi
float32
float64
nan
inf
-inf
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Automatic type conversion

Number types are automatically converted in a way to preserve the best precision.
Integers are preferred to rational numbers and rational numbers are preferred
to floating point numbers.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
1+2/3
1/3+2/3
(2/3) * 0.5
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Display mode

By default only the raw value of the result is displayed.
The user can activate additional display modes by selecting:

- the integral base (`dec`, `hex`, `oct`, `bin`)
- the number of bits (`8`, `16`, `32`, `64`)
- the IEEE 754 representation of floating point numbers (`float32`, `float64`)
- `reset` resets the display mode

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
42424242
dec8            # 8 bit decimal numbers
hex16           # 16 bit hexadecimal numbers
oct32           # 32 bit octal numbers
bin64           # 64 bit binary numbers
reset           # raw decimal value only
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Handy Calc automatically activates some display modes under some circonstances:

- integer entered in a specific base
- usage of a bitwise operator in an expression

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
4               # only the default display mode
0b100           # this number activates the binary display mode
1<<10           # this operator activates the hexadecimal display mode
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Strings

Handy Calc has a limited support for strings.

Strings can be concatenated, duplicated and produced by converting numbers:

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
"abc" + "def"
"abc" * 3
"pi = " + pi + "; e = " ++ e
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Booleans

Boolean values can be used in conditional and boolean expressions.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
true
false
true and false
1+1 == 2
1+1==2 ? "ok" : "bug"
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Operators

### Arithmetic operators

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = 12
-x
+x
x + 1
x - 1
x * 2
x / 5
x // 5                  # integral division
x % 5                   # integral remainder (Euclidean division)
x ** 2
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Bitwise operators

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
bin16
~1                      # bitwise complement
1 | 4                   # bitwise or
0b1100 ^ 0b0110         # bitwise exclusive or
0b1100 & 0b0110         # bitwise and
1 << 10                 # left shift
1024 >> 1               # right shift
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Boolean operators

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
not true
true or false
true xor false
true and false
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Comparison operators

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
12 < 13
12 <= 13
12 > 13
12 >= 13
12 == 13
12 != 13
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Operator precedence

From highest to lowest precedence:

Operator family             Syntax
--------------------------- -----------------------------
Precedence overloading      `(...)`
Function evaluation         `f(...)`
Exponentiation              `x**y`
Unary operators             `+x`, `-y`, `~z`
Multiplicative operators    `*` `/` `%` `&` `<<` `>>`
Additive operators          `+` `-` `|` `^`
Relational operators        `<` `<=` `>` `>=` `==` `!=`
Logical not                 `not x`
Logical and                 `and`
Logical or                  `or` `xor`
Ternary operator            `x ? y : z`
Assignment                  `x = y`
Blocks                      `expr1; ...; exprn`

## Variables

Handy Calc can define and reuse variables.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = 1
y = 2
x+y
y = 3
x+y
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Functions

Handy Calc can also define functions.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
f(x) = 2 * x
f(5)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Functions can be defined with multiple statements and be recursive.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
fib(n) = (f1=fib(n-1); f2=fib(n-2); n<2 ? 1 : f1+f2)
fib(1)
fib(10)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

You can see in the previous example that the evaluation is lazy!
Thanks to laziness, functions can also be mutually recursive.

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
isEven(n) = n == 0 ? true : isOdd(n-1)
isOdd(n) = n == 0 ? false : isEven(n-1)
isEven(10)
isOdd(10)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Builtin functions

### Type conversion

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
int(pi)                     # Integral part
float(2/3)                  # Conversion to floating point numbers
rat(pi)                     # Rational approximation
rat(pi, 1e-2)               # Rational approximation with a given precision
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Math

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = pi; y = e; b = 3

abs(x)                      # absolute value of x
ceil(x)                     # smallest integer larger than or equal to x
floor(x)                    # largest integer smaller than or equal to x
round(x)                    # round to the nearest integer
trunc(x)                    # round toward zero
mantissa(x)                 # m such that x = m2e, |m| is in [0.5, 1[
exponent(x)                 # e such that x = m2e, e is an integer
int(x)                      # integral part of x
fract(x)                    # fractional part of x
min(x, y)                   # minimum value among its arguments
max(x, y)                   # maximum value among its arguments

sqr(x)                      # square of x (x**2)
sqrt(x)                     # square root of x (x**0.5)
cbrt(x)                     # cubic root of x (x**(1/3))

cos(x)                      # trigonometric functions
acos(x)
cosh(x)
sin(x)
asin(x)
sinh(x)
tan(x)
atan(x)
tanh(x)
atan(y, x)                  # arc tangent of y/x (in radians)
atan2(y, x)                 # arc tangent of y/x (in radians)
deg(x)                      # angle x (given in radians) in degrees
rad(x)                      # angle x (given in degrees) in radians

exp(x)                      # e**x
log(x)                      # logarithm of x in base e
ln(x)                       # logarithm of x in base e
log10(x)                    # logarithm of x in base 10
log2(x)                     # logarithm of x in base 2
log(b, x)                   # logarithm of x in base b
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### IEEE 754 representation

#### 32 bit numbers

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = pi; n = 0x402df854

float32
float2ieee(x)               # IEEE 754 representation of x (32 bits)
ieee2float(n)               # 32 bit float value of the IEEE 754 integer n
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#### 64 bit numbers

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = pi; n = 0x4005bf0a8b145769

float64
double2ieee(x)              # IEEE 754 representation of x (64 bits)
ieee2double(n)              # 64 bit float value of the IEEE 754 integer n
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### Specific values

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
x = pi

isfinite(x)                 # true if x is finite
isinf(x)                    # true if x is infinite
isnan(x)                    # true if x is not a number
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Other commands

Other commands              Description
--------------------------- ---------------------------
bye, exit, quit             quit
ascii                       print an ASCII table
help                        print this help
version                     print the version number

Online help
===========

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ {cmd="{{hcalc}}"}
help
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
