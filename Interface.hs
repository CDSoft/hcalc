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

{- The module Interface implements the command line interface
of the calculator.
-}

{-# LANGUAGE CPP #-}

module Interface(repl) where

import Expression
import Help
import Parser
import PrettyPrint

-- REPL: interactive calculator
repl :: (FilePath, String) -> [String] -> [String]
repl (name, ini) exprs = welcome : loop stIni None exprs
    where
        (stIni@(confIni, _), vIni) = eval emptyState $ parse ini
        welcome = shortHelp
                  ++ "\nLoading "++name
                  ++ case vIni of
                        err@(E _) -> "\n" ++ pp confIni err
                        _ -> ""
        loop _ _ [] = []
        loop st prev (x:xs) = case x' of
                Bye _ -> []
                _ -> pp conf' prev' : loop st' prev' xs
            where
                (st'@(conf', _), x') = eval st $ parse x
                prev' = case x' of
                    Previous -> prev
                    _ -> x'
