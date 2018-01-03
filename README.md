<!--
Handy Calc
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
-->

# Introduction

Handy Calc is a simple yet powerful calculator. Unlike most of other
calculators, Handy Calc is based on a textual interface. It may seem a
bit spartan and outdated but entering expressions with the keyboard is
way easier than with a mouse. And you get nice editing features for free
(edition, copy/paste, history, …).

Handy Calc is also an application example for the
[FUN](http://fun.cdsoft.fr) project. Its development process and methods
are based on:

  - Haskell
  - static and strong typing
  - extreme compiler checks
  - executable specification with embedded tests
  - unit testing to ease evolutions and non regression checks
  - code coverage to measure the completeness of the tests

So Handy Calc is supposed to be better and safer than its predecessor
([Calculadoira](http://cdsoft.fr/calculadoira)).

If you like Handy Calc, please consider supporting my
[FUN](http://fun.cdsoft.fr) project.

You can also contribute to [Handy Calc on
GitHub](http://github.com/CDSoft/hcalc).

# License

    Handy Calc
    (C) 2016, 2017, 2018 Christophe Delord
    http://cdsoft.fr/hcalc
    
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
    
    Handy Calc is powered by Haskell.

# Download and installation

The current version is [Handy
Calc 1.0.5](http://cdsoft.fr/hcalc/HandyCalc-1.0.5.tgz)

**Installation from sources on Linux or Windows:**

  - Prerequisites
      - [The Haskell Tool
        Stack](https://docs.haskellstack.org/en/latest/README/)
      - Cygwin or MSYS/Mingw properly installed on Windows
      - [PP](http://cdsoft.fr/pp) and [Pandoc](http://pandoc.org/) to
        generate the documentation (optional)
  - Download
    [HandyCalc-1.0.5.tgz](http://cdsoft.fr/hcalc/HandyCalc-1.0.5.tgz)
  - Unpack HandyCalc-1.0.5.tgz
  - Run make
      - `make` compiles `hcalc`
      - `make install` installs `hcalc` in `~/.local/bin`
      - `make doc` generates the documentation in `doc`
      - `make test` runs the non regression tests

**Binaries:**

The binaries are not provided anymore. Use the source Luke\!

**Notes:**

For a better user experience on Linux, it is recommended to use Handy
Calc with
[`rlwrap`](http://utopia.knoware.nl/~hlub/rlwrap/rlwrap.man.html) (e.g.
`rlwrap hcalc`). `rlwrap` will give Handy Calc nice editing features.

I use a keyboard shortcut to start Handy Calc in a
    terminal:

``` .bash
urxvt +sb -T hCalc -e rlwrap ~/bin/hcalc
```

# Screenshot

    +---------------------------------------------------------------------+
    |       H A N D Y   C A L C       |     v 1.0.5     | cdsoft.fr/hcalc |
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

# Command line usage

Handy Calc can be used on the command line. Each argument is considered
as an expression to be evaluated. Only the value of the last expression
is printed.

    $ hcalc "x = 21" "y = 2" "x * y"
    =       42

# Interactive usage

The main usage of Handy Calc is by interacting in a terminal.
Expressions are entered with the keyboard, evaluated and the result is
printed. The next section lists all the operators and functions provided
by Handy Calc.

A typical interactive session looks like this:

``` 
$ hcalc
+---------------------------------------------------------------------+
|       H A N D Y   C A L C       |     v 1.0.5     | cdsoft.fr/hcalc |
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

: x = 21


: y = 2


: (x * y) ** 2
=       1764

: 
```

# User’s manual

## Numbers

### Integers

Integers can be decimal, hexadecimal, octal or binary numbers:

``` 
: 42
=       42

: 0x24
=       36
hex     0x24

: 0o37
=       31
hex     0x1f
oct     0o37

: 0b1010
=       10
hex     0xa
oct     0o12
bin     0b1010

```

### Rational numbers

Rational numbers can be used to make *exact* computations instead of
using floating point numbers.

``` 
: 1 + 2/3
=       5/3
~       1.6666666666666667

```

Some functions don’t support rational numbers and will produce floating
point numbers.

``` 
: 1/2 + cos(0)
=       1.5

```

### Floating point numbers

Floating point numbers are single (32 bit) or double (64 bits) precision
floating point numbers.

They are represented internally by 64 bit numbers but can be converted
to 32 bit numbers as well as to their IEEE 754 representation.

``` 
: 3.14
=       3.14

: 1.23e-6
=       1.23e-6

: e
=       2.718281828459045

: pi
=       3.141592653589793

: float32
=       3.141592653589793
flt32   3.1415927 <=> 0x40490fdb

: float64
=       3.141592653589793
flt64   3.141592653589793 <=> 0x400921fb54442d18

: nan
=       NaN
flt64   NaN <=> 0xfff8000000000000

: inf
=       Infinity
flt64   Infinity <=> 0x7ff0000000000000

: -inf
=       -Infinity
flt64   -Infinity <=> 0xfff0000000000000

```

### Automatic type conversion

Number types are automatically converted in a way to preserve the best
precision. Integers are prefered to rational numbers and rational
numbers are prefered to floating point numbers.

``` 
: 1+2/3
=       5/3
~       1.6666666666666667

: 1/3+2/3
=       1

: (2/3) * 0.5
=       0.3333333333333333

```

### Display mode

By default only the raw value of the result is displayed. The user can
activate additional display modes by selecting:

  - the integral base (`dec`, `hex`, `oct`, `bin`)
  - the number of bits (`8`, `16`, `32`, `64`)
  - the IEEE 754 representation of floating point numbers (`float32`,
    `float64`)
  - `reset` resets the display mode

<!-- end list -->

``` 
: 42424242
=       42424242

: dec8            # 8 bit decimal numbers
=       42424242
dec8    178

: hex16           # 16 bit hexadecimal numbers
=       42424242
dec16   22450
hex16   0x57b2

: oct32           # 32 bit octal numbers
=       42424242
dec32   0042424242
hex32   0x028757b2
oct32   0o00241653662

: bin64           # 64 bit binary numbers
=       42424242
dec64   00000000000042424242
hex64   0x00000000028757b2
oct64   0o0000000000000241653662
bin64   0b0000000000000000000000000000000000000010100001110101011110110010

: reset           # raw decimal value only
=       42424242

```

Handy Calc automatically activates some display modes under some
circonstances:

  - integer entered in a specific base
  - usage of a bitwise operator in an expression

<!-- end list -->

``` 
: 4               # only the default display mode
=       4

: 0b100           # this number activates the binary display mode
=       4
bin     0b100

: 1<<10           # this operator activates the hexadecimal display mode
=       1024
hex     0x400
bin     0b10000000000

```

## Strings

Handy Calc has a limited support for strings.

Strings can be concatenated, duplicated and produced by converting
numbers:

``` 
: "abc" + "def"
=       "abcdef"

: "abc" * 3
=       "abcabcabc"

: "pi = " + pi + "; e = " ++ e
=       "pi = 3.141592653589793; e = 2.718281828459045"

```

## Booleans

Boolean values can be used in conditional and boolean expressions.

``` 
: true
=       true

: false
=       false

: true and false
=       false

: 1+1 == 2
=       true

: 1+1==2 ? "ok" : "bug"
=       "ok"

```

## Operators

### Arithmetic operators

``` 
: x = 12


: -x
=       -12

: +x
=       12

: x + 1
=       13

: x - 1
=       11

: x * 2
=       24

: x / 5
=       12/5
~       2.4

: x // 5                  # integral division
=       2

: x % 5                   # integral remainder (Euclidean division)
=       2

: x ** 2
=       144

```

### Bitwise operators

``` 
: bin16


: ~1                      # bitwise complement
=       -2
hex16   0xfffe
bin16   0b1111111111111110

: 1 | 4                   # bitwise or
=       5
hex16   0x0005
bin16   0b0000000000000101

: 0b1100 ^ 0b0110         # bitwise exclusive or
=       10
hex16   0x000a
bin16   0b0000000000001010

: 0b1100 & 0b0110         # bitwise and
=       4
hex16   0x0004
bin16   0b0000000000000100

: 1 << 10                 # left shift
=       1024
hex16   0x0400
bin16   0b0000010000000000

: 1024 >> 1               # right shift
=       512
hex16   0x0200
bin16   0b0000001000000000

```

### Boolean operators

``` 
: not true
=       false

: true or false
=       true

: true xor false
=       true

: true and false
=       false

```

### Comparison operators

``` 
: 12 < 13
=       true

: 12 <= 13
=       true

: 12 > 13
=       false

: 12 >= 13
=       false

: 12 == 13
=       false

: 12 != 13
=       true

```

### Operator precedence

From highest to lowest precedence:

| Operator family          | Syntax                      |
| :----------------------- | :-------------------------- |
| Precedence overloading   | `(...)`                     |
| Function evaluation      | `f(...)`                    |
| Exponentiation           | `x**y`                      |
| Unary operators          | `+x`, `-y`, `~z`            |
| Multiplicative operators | `*` `/` `%` `&` `<<` `>>`   |
| Additive operators       | `+` `-` `\|` `^`            |
| Relational operators     | `<` `<=` `>` `>=` `==` `!=` |
| Logical not              | `not x`                     |
| Logical and              | `and`                       |
| Logical or               | `or` `xor`                  |
| Ternary operator         | `x ? y : z`                 |
| Assignment               | `x = y`                     |
| Blocks                   | `expr1; ...; exprn`         |

## Variables

Handy Calc can define and reuse variables.

``` 
: x = 1


: y = 2


: x+y
=       3

: y = 3


: x+y
=       4

```

## Functions

Handy Calc can also define functions.

``` 
: f(x) = 2 * x


: f(5)
=       10

```

Functions can be defined with multiple statements and be recursive.

``` 
: fib(n) = (f1=fib(n-1); f2=fib(n-2); n<2 ? 1 : f1+f2)


: fib(1)
=       1

: fib(10)
=       89

```

You can see in the previous example that the evaluation is lazy\! Thanks
to lazyness, functions can also be mutually recursive.

``` 
: isEven(n) = n == 0 ? true : isOdd(n-1)


: isOdd(n) = n == 0 ? false : isEven(n-1)


: isEven(10)
=       true

: isOdd(10)
=       false

```

## Builtin functions

### Type conversion

``` 
: int(pi)                     # Integral part
=       3

: float(2/3)                  # Conversion to floating point numbers
=       0.6666666666666666

: rat(pi)                     # Rational approximation
=       884279719003555/281474976710656
~       3.141592653589793

: rat(pi, 1e-2)               # Rational approximation with a given precision
=       22/7
~       3.142857142857143

```

### Math

``` 
: x = pi; y = e; b = 3


: abs(x)                      # absolute value of x
=       3.141592653589793

: ceil(x)                     # smallest integer larger than or equal to x
=       4

: floor(x)                    # largest integer smaller than or equal to x
=       3

: round(x)                    # round to the nearest integer
=       3

: trunc(x)                    # round toward zero
=       3

: mantissa(x)                 # m such that x = m2e, |m| is in [0.5, 1[
=       0.7853981633974483

: exponent(x)                 # e such that x = m2e, e is an integer
=       2

: int(x)                      # integral part of x
=       3

: fract(x)                    # fractional part of x
=       0.14159265358979312

: min(x, y)                   # minimum value among its arguments
=       2.718281828459045

: max(x, y)                   # maximum value among its arguments
=       3.141592653589793

: sqr(x)                      # square of x (x**2)
=       9.869604401089358

: sqrt(x)                     # square root of x (x**0.5)
=       1.7724538509055159

: cbrt(x)                     # cubic root of x (x**(1/3))
=       1.4645918875615231

: cos(x)                      # trigonometric functions
=       -1.0

: acos(x)
=       NaN

: cosh(x)
=       11.591953275521519

: sin(x)
=       1.2246467991473532e-16

: asin(x)
=       NaN

: sinh(x)
=       11.548739357257748

: tan(x)
=       -1.2246467991473532e-16

: atan(x)
=       1.2626272556789118

: tanh(x)
=       0.99627207622075

: atan(y, x)                  # arc tangent of y/x (in radians)
=       0.7132845404390503

: atan2(y, x)                 # arc tangent of y/x (in radians)
=       0.7132845404390503

: deg(x)                      # angle x (given in radians) in degrees
=       180.0

: rad(x)                      # angle x (given in degrees) in radians
=       5.483113556160755e-2

: exp(x)                      # e**x
=       23.140692632779267

: log(x)                      # logarithm of x in base e
=       1.1447298858494002

: ln(x)                       # logarithm of x in base e
=       1.1447298858494002

: log10(x)                    # logarithm of x in base 10
=       0.4971498726941338

: log2(x)                     # logarithm of x in base 2
=       1.651496129472319

: log(b, x)                   # logarithm of x in base b
=       1.0419780459921857

```

### IEEE 754 representation

#### 32 bit numbers

``` 
: x = pi; n = 0x402df854


: float32


: float2ieee(x)               # IEEE 754 representation of x (32 bits)
=       1078530011
flt32   3.1415927 <=> 0x40490fdb

: ieee2float(n)               # 32 bit float value of the IEEE 754 integer n
=       2.7182817459106445
flt32   2.7182817 <=> 0x402df854

```

#### 64 bit numbers

``` 
: x = pi; n = 0x4005bf0a8b145769


: float64


: double2ieee(x)              # IEEE 754 representation of x (64 bits)
=       4614256656552045848
flt64   3.141592653589793 <=> 0x400921fb54442d18

: ieee2double(n)              # 64 bit float value of the IEEE 754 integer n
=       2.718281828459045
flt64   2.718281828459045 <=> 0x4005bf0a8b145769

```

### Specific values

``` 
: x = pi


: isfinite(x)                 # true if x is finite
=       true

: isinf(x)                    # true if x is infinite
=       false

: isnan(x)                    # true if x is not a number
=       false

```

## Other commands

| Other commands  | Description              |
| :-------------- | :----------------------- |
| bye, exit, quit | quit                     |
| ascii           | print an ASCII table     |
| help            | print this help          |
| version         | print the version number |

# Online help

``` 
: help

+---------------------------------------------------------------------+
|       H A N D Y   C A L C       |     v 1.0.5     | cdsoft.fr/hcalc |
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

Constants                   Value
=========================== ===============================================

nan                         Not a Number
inf                         Infinite
pi                          3.1415927
e                           2.7182817

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

Handy Calc 1.0.5
(C) 2016, 2017, 2018 Christophe Delord
http://cdsoft.fr/hcalc


```
