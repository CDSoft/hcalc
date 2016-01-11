module Help(welcome, help) where

import Data.List

dates :: [Int]
dates = [2016]

welcome :: String
welcome = unlines [ "The ultimate calculator"
                  , "(C) " ++ intercalate ", " (map show dates)  ++ " Christophe Delord"
                  , "http://cdsoft.fr/ucalc"
                  , ""
                  , "Ultimate Calc is free software: you can redistribute it and/or modify"
                  , "it under the terms of the GNU General Public License as published"
                  , "by the Free Software Foundation, either version 3 of the License, or"
                  , "(at your option) any later version."
                  , ""
                  , "Ultimate Calc is distributed in the hope that it will be useful,"
                  , "but WITHOUT ANY WARRANTY; without even the implied warranty of"
                  , "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the"
                  , "GNU General Public License for more details."
                  , ""
                  , "You should have received a copy of the GNU General Public License"
                  , "along with Ultimate Calc.  If not, see <http://www.gnu.org/licenses/>."
                  , ""
                  , "Ultimate Calc is powered by Haskell."
                  , ""
                  , "NOTICE: Ultimate Calc is being developed. It may be incomplete and buggy."
                  ]

help :: String
help = unlines [ "help..."
               ]
