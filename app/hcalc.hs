{- Handy Calc
Copyright (C) 2016, 2017, 2018 Christophe Delord
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
-}

{- The module Main contains all the non pure IO operations.
-}

{-# LANGUAGE CPP #-}

module Main(main) where

import Help
import Interface
import PrettyPrint
import qualified Version as V

import Control.Monad
import Data.Char
import System.Directory
import System.Environment
import System.IO

#ifdef linux_HOST_OS
import System.Posix.Process
#endif
#ifdef mingw32_HOST_OS
import System.Process
#endif

main :: IO ()
main = do
    args <- getArgs
    ini <- getIni
    case args of
        [] -> initConsole >> interact' (repl ini)
        _ -> putStrLn $ last $ repl ini args

#ifdef linux_HOST_OS
initConsole :: IO ()
initConsole = nice 19
#endif

-- some cosmetics for the windows console
#ifdef mingw32_HOST_OS
initConsole :: IO ()
initConsole = do
    tty <- hIsTerminalDevice stdin
    when tty $ do
        callCommand $ "title " ++ V.shortName
        callCommand "color f0"
#endif

-- read the configuration file.
-- The name of the configuration file is the name of the executable
-- with the extension .ini
getIni :: IO (FilePath, String)
getIni = do
    name <- getXdgDirectory XdgConfig $ map toLower V.shortName ++ ".ini"
    found <- doesFileExist name
    if found
        -- if it exists, just read it
        then do h <- openFile name ReadMode
                hSetEncoding h utf8
                ini <- hGetContents h
                return (name, ini)
        -- otherwise create a sample one
        else do writeFile name defaultIni
                return (name, defaultIni)

-- interact' is very similar to interact but the input is split by lines
interact' :: ([String] -> [String]) -> IO ()
interact' f = do
    input <- getContents
    let inputLines = lines input
#ifdef linux_HOST_OS
    writeLines inputLines (f inputLines)
#else
    writeLines (f inputLines)
#endif

-- write the results of the calculator,
-- as well as the prompt for the next input
#ifdef linux_HOST_OS
writeLines :: [String] -> [String] -> IO ()
writeLines inputs outputs = forM_ (zip ("":inputs) outputs) (\(input, output) -> do
        tty <- hIsTerminalDevice stdin
        unless tty $ hFlush stdout >> putStrLn input
        hFlush stdout
        putStrLn output
        putStr $ "\n"++prompt ":"
        hFlush stdout
    )
#else
writeLines :: [String] -> IO ()
writeLines outputs = forM_ outputs (\output -> do
        hFlush stdout
        putStrLn output
        putStr $ "\n"++prompt ":"
        hFlush stdout
    )
#endif
