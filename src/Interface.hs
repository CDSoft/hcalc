{- Handy Calc
Copyright (C) 2016-2020 Christophe Delord
http://cdelord.fr/hcalc

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

import qualified Data.Map as Map

import Expression
import Help
import Parser
import PrettyPrint

-- REPL: interactive calculator
repl :: (FilePath, String) -> [String] -> [String]
repl (name, ini) exprs = welcome : loop (nextIterationState stIni) None exprs
    where

        (stIni@(confIni, _, _, _), vIni) = eval emptyState $ parse ini

        welcome = shortHelp
                  ++ "\nLoading "++name
                  ++ case vIni of
                        err@(E _) -> "\n" ++ pp confIni err
                        _ -> ""

        loop :: State -> Expr -> [String] -> [String]
        loop _ _ [] = []
        loop st prev (x:xs) = case x' of
                Bye _ -> []
                _ -> pp conf' prev' : loop (nextIterationState st') prev' xs
            where
                (st'@(conf', _, _, _), x') = eval st $ parse x
                prev' = case x' of
                    Previous -> prev
                    _ -> x'

        -- In the interactive loop, the definitions stored in the local stack
        -- must be saved to the heap to be available in subsequent iterations.
        nextIterationState :: State -> State
        nextIterationState (conf, heap, _, stack) = (conf, heap', [], [])
            where
                heap' = Map.union (Map.fromList stack) heap
