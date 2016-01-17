{- Ultimate Calc
Copyright (C) 2016 Christophe Delord
http://cdsoft.fr/ucalc

This file is part of Ultimate Calc.

Ultimate Calc is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published
by the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

Ultimate Calc is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Ultimate Calc.  If not, see <http://www.gnu.org/licenses/>.
-}

{- The module Interface implements the command line interface
of the calculator.
-}

{-# LANGUAGE CPP #-}

module Interface(doArgs) where

import Expression
import Parser
import PrettyPrint
import Help

import Data.List
import System.Environment
import System.IO
import System.FilePath
import System.Directory

#ifdef linux_HOST_OS
import Data.Char
import System.Console.Readline
import System.Exit
import System.Posix.Process
#endif
#ifdef mingw32_HOST_OS
import System.Process
#endif

doArgs :: [String] -> IO ()
doArgs [] = doREPL
doArgs exprs = doCLI $ intercalate "; " exprs

-- command line interface: evaluate the expressions given on the command line
doCLI :: String -> IO ()
doCLI exprs = do
    state <- loadIni emptyState
    let e = parse exprs
    let ((conf, _), v) = eval state e
    case v of
        Bye _ -> return ()
        _ -> putStrLn . pp conf $ v

-- REPL: interactive calculator
doREPL :: IO ()
doREPL = do
    initProcess
    putStrLn shortHelp
    state <- loadIni emptyState
    repl state None
    where
        repl :: State -> Expr -> IO ()
        repl st prev = do
            putStrLn ""
            line <- readLine (prompt ":")
            st' <- loadIni st
            let e = parse line
            let (st''@(conf, _), v) = eval st' e
            let prev' = case v of
                            Previous -> prev
                            _ -> v
            case v of
                Bye _ -> return ()
                _ -> do
                    putStrLn . pp conf $ prev'
                    repl st'' prev'

-- some cosmetics for the windows console
#ifdef mingw32_HOST_OS
initProcess :: IO ()
initProcess = do
    -- _ <- runCommand "title uCalc"
    -- _ <- runCommand "color f0"
    callCommand "title uCalc"
    callCommand "color f0"
#endif

#ifdef linux_HOST_OS
initProcess :: IO ()
initProcess = nice 19
#endif

-- on Linux we use libreadline to get a better user experience
#ifdef linux_HOST_OS
readLine :: String -> IO String
readLine inputPrompt = do
    hFlush stdout
    maybeLine <- readline inputPrompt
    hFlush stdout
    case maybeLine of
        Nothing     -> exitSuccess
        Just line   -> if not $ null $ filter (not.isSpace) line then
                            do  addHistory line
                                return line
                       else
                            do  return ""
#endif

-- on Windows we use the default console edition capabilities
#ifdef mingw32_HOST_OS
readLine :: String -> IO String
readLine inputPrompt = do
    hFlush stdout
    putStr inputPrompt
    hFlush stdout
    line <- getLine
    return line
#endif

-- name of the configuration file
-- the configuration file has got the name of the executable with the .ini extension
iniName :: IO String
iniName = do
    path <- getExecutablePath
    return $ replaceExtension path ".ini"

-- loadIni load the .ini file if it exists and if it has been modified
-- since lst time it was loaded.
-- It returns a new state in which the .ini file has been evaluated.
-- The last modification time is stored in the state.
loadIni :: State -> IO State
loadIni st@(conf, _) = do
    name <- iniName
    iniFileFound <- doesFileExist name
    if not iniFileFound
        then return st
        else do
            t1 <- getModificationTime name
            case mtime conf of
                    Nothing -> load name t1
                    Just t0 | t0 < t1 -> load name t1
                            | otherwise -> return st
    where
        load fName t = do
            putStrLn $ "Loading " ++ takeFileName fName
            h <- openFile fName ReadMode
            hSetEncoding h utf8
            ini <- hGetContents h
            let (st', v) = eval st $ parse ini
            case v of
                E err -> putStrLn $ "Error while loading " ++ takeFileName fName ++ ": " ++ err
                _ -> return ()
            return $ setMTime st' t
