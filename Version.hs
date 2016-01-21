{- Handy Calc
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
-}

{- This module defines the current version
-}

module Version(name, shortName, version, dates, tag) where

import Data.List

name :: String
name = "Handy Calc"

shortName :: String
shortName = "hCalc"

version :: [Int]
version = [0, 0, 2]

dates :: [Int]
dates = [2016]

tag :: String
tag = name ++ " " ++ intercalate "." (map show version)