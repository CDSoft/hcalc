% Handy Calc Manual
% Christophe Delord
% \MDATE

<!--
Handy Calc
Copyright (C) 2016 Christophe Delord
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

Introduction
============

Handy Calc is a simple yet powerful calculator.
Unlike most of the calculators, Handy Calc is based on a textual interface.
It may seem a bit spartan and outdated but entering expressions with the keyboard is way easier than with a mouse.
And you get nice editing features for free (edition, copy/paste, history, ...).

Handy Calc is also an application example for the [FUN](http://fun.cdsoft.fr) project.
Its development process and methods are based on:

- Haskell
- static and strong typing
- extreme compiler checks
- executable specification with embedded tests
- unit testing to ease evolutions and non regression checks
- code coverage to measure the completeness of the tests

So Handy Calc is supposed to be better and safer than its predecessor ([Calculadoira](http://cdsoft.fr/calculadoira)).

If you like Handy Calc, please consider supporting my [FUN](http://fun.cdsoft.fr) project.

License
=======

~~~~~~~~~~~~~~~~~~
~~~~~~~~ sh
hcalc license
~~~~~~~~
~~~~~~~~~~~~~~~~~~

Download and installation
=========================

\def{TAR}{\exec{hcalc version | sed 's/ //' | sed 's/ /-/'}.tgz}
\def{URL}{http://cdsoft.fr/hcalc/!TAR}
\def{VERSION}{\exec{hcalc version}}

\def{LINUX}{[hcalc](http://cdsoft.fr/hcalc/hcalc)}
\def{WIN}{[hcalc.exe](http://cdsoft.fr/hcalc/hcalc.exe)}

\def{PP}{[PP](http://cdsoft.fr/pp)}
\def{Pandoc}{[Pandoc](http://pandoc.org/)}
\def{Haskell}{[Haskell](https://www.haskell.org/)}

The current version is [!VERSION](!URL)

**Installation from sources on Linux or Windows:**

- Prerequisites
    - !Haskell
    - Cygwin or MSYS/Mingw properly installed on Windows
    - !PP and !Pandoc to generate the documentation (optional)
- Download [!TAR](!URL)
- Unpack !TAR
- Run make
    - `make bin` generates the binary executble in `bin`
    - `make doc` generates the documentation in `doc`
    - `make test` runs the non regression tests

**Binaries:**

- Precompiled 32 bit binary for Windows: !WIN
- Precompiled 64 bit binary for Linux: !LINUX

**Notes:**

- For a better user experience on Linux, it is recommended to use Handy Calc with `rlwrap` (e.g. `rlwrap hcalc`). `rlwrap` will give Handy Calc nice editing features.
- The binaries may or may not work on your specific OS version. Compiling the sources is the prefered way to get Handy Calc work.

Screenshot
==========

~~~~~~~~~~~~~~~~~~
~~~~~~~~ sh
hcalc bye | grep -v ^Loading
~~~~~~~~
~~~~~~~~~~~~~~~~~~

Command line usage
==================

Handy Calc can be used on the command line. Each argument is considered as an expression to be evaluated. Only the value of the last expression is printed.

~~~~~~~~~~~~~~~~~~
$ hcalc "x = 21" "y = 2" "x*y"
\exec(hcalc "x = 21" "y = 2" "x*y")
~~~~~~~~~~~~~~~~~~

Interactive usage
=================

The main usage of Handy Calc is by interacting in a terminal.
Expressions are entered with the keyboard, evaluated and the result is printed.
The next section lists all the operators and functions provided by Handy Calc.

A typical interactive looks like this:

~~~~~~~~~~~~~~~
$ hcalc
\exec(hcalc help | head -20)

: x = 21

: y = 2

: (x*y) ** 2
\exec(hcalc x=21 y=2 "(x*y)**2")

: bye
~~~~~~~~~~~~~~~

User's manual
=============

\begin{code}

-- The examples given in the user's manual are actually executed to check that hcalc produce the expected outputs.

{-# LANGUAGE  QuasiQuotes #-}

import Interface
import Data.String.Here.Interpolated
import System.Exit

-- The whole scenario is in a string (hCalcTest)
hCalcTests :: String
hCalcTests = [i|
\end{code}

**TODO**: write a real user's manual!

`````````````````````````````````````````````````````````````````````
\begin{code}
: 1+1
=       2

: cos(pi)
=       -1.0

: float32
=       -1.0
flt32   -1.0 <=> 0xbf800000

: float64
=       -1.0
flt64   -1.0 <=> 0xbff0000000000000

\end{code}
`````````````````````````````````````````````````````````````````````

\def{ex}(
~~~~~~~~~~~~~~~~~~~~~~~~~~
!1
~~~~~~~~~~~~~~~~~~~~~~~~~~

\begin{code}
: 2*3
=       6
\end{code}
)

un example :

!ex[
: 2*3
=       6
]

\begin{code}

|]

main :: IO ()
main = case check hCalcTests of
    (_, 0, total) -> putStrLn ("All tests passed (" ++ show total ++ " tests)")
    (d, n, total) -> putStrLn (unlines d) >> putStrLn ("Test failed: " ++ show n ++ " / " ++ show total) >> print hCalcTests >> exitFailure

check :: String -> ([String], Int, Int)
check test = (differences, errors, total)
    where
        differences = diff inputs outputs outputs'
        errors = sum [1 | d <- differences, not (null d), head d /= ' ', head d /= ':']
        total = length inputs

        (inputs, outputs) = parse (lines test) [] []
        outputs' = map lines $ drop 1 $ repl ("", "") (map cmd inputs)

        parse :: [String] -> [String] -> [[String]] -> ([String], [[String]])
        parse (x:xs) ins outs
            | isInput x = parseOut xs (x:ins) outs []
            | isEmpty x = parse xs ins outs
            | otherwise = error "Invalid test"
        parse [] ins outs = (reverse ins, reverse outs)

        parseOut :: [String] -> [String] -> [[String]] -> [String] -> ([String], [[String]])
        parseOut (x:xs) ins outs current
            | isInput x = parse (x:xs) ins (reverse current:outs)
            | isEmpty x = parseOut xs ins outs current
            | otherwise = parseOut xs ins outs (x:current)
        parseOut [] ins outs current = parse [] ins (reverse current:outs)

        diff :: [String] -> [[String]] -> [[String]] -> [String]
        diff (inp:ins) (out:outs) (out':outs') = inp : diff' out out' ++ diff ins outs outs'
        diff _ _ _ = []

        diff' :: [String] -> [String] -> [String]
        diff' (x:xs) (y:ys)
            | trim x == trim y  = ("   " ++ y                                                      ) : diff' xs ys
            | otherwise         = ("=>1" ++ y ++ replicate (w - length y) ' ' ++ "   <=/=>   " ++ x) : diff' xs ys
        diff' [] (y:ys)         = ("=>2" ++ y ++ replicate (w - length y) ' ' ++ "   <=/=>   "     ) : diff' [] ys
        diff' (x:xs) []         = ("=>3"      ++ replicate  w             ' ' ++ "   <=/=>   " ++ x) : diff' xs []
        diff' [] [] = []

        w = maximum (map length (inputs ++ concat outputs'))

        cmd :: String -> String
        cmd (' ':cs) = cmd cs
        cmd (':':cs) = trim cs
        cmd _ = ""

        isInput :: String -> Bool
        isInput (' ':cs) = isInput cs
        isInput (':':_) = True
        isInput _ = False

        isEmpty :: String -> Bool
        isEmpty s = null (trim s)

        trim :: String -> String
        trim (' ':cs) = trim cs
        trim (c:cs) = c:trim' cs
            where
                trim' (' ':' ':cs') = trim' (' ':cs')
                trim' " " = ""
                trim' (c':cs') = c':trim' cs'
                trim' [] = []
        trim [] = []

\end{code}
