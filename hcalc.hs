{- Calculator in Haskell

The code constains all the non regression tests
as well as the documentation.

This is a exercice to demonstrate an incremental test driven process:
- write a test case
- write the code until it passes the test case
- write another test case...

usage:
calc -l displays the license
calc -h displays some help
calc -t runs the non regression tests
calc expr computes expr and exit
calc enters the CLI mode

-}

-- TODO : evaluation : env global et local
-- si l'env local est vide, on est en global => les définitions vont dans l'env global
-- si l'env local n'est pas vide, les définitions sont dans l'env local

-- TODO : strings et tuples

{-# LANGUAGE CPP #-}

module Main(main) where

import Interface
--import Expression
--import Parser
import PrettyPrint
--import Help

import System.Environment
--import GHC.IO.Device

--import Data.List
import Control.Monad
--import System.Environment
import System.IO
import System.FilePath
import System.Directory

#ifdef linux_HOST_OS
--import Data.Char
--import System.Console.Readline
--import System.Exit
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
        [] -> initConsole >> (interact' $ repl ini)
        _ -> putStrLn $ last $ repl ini args

-- some cosmetics for the windows console
#ifdef mingw32_HOST_OS
initConsole :: IO ()
initConsole = do
    tty <- hIsTerminalDevice stdin
    when tty $ do
        callCommand "title hCalc"
        callCommand "color f0"
#endif

#ifdef linux_HOST_OS
initConsole :: IO ()
initConsole = nice 19
#endif

getIni :: IO (FilePath, String)
getIni = do
    path <- getExecutablePath
    let name = replaceExtension path ".ini"
    found <- doesFileExist name
    if found
        then do h <- openFile name ReadMode
                hSetEncoding h utf8
                ini <- hGetContents h
                return (name, ini)
        else return (name, "")

interact' :: ([String] -> [String]) -> IO ()
interact' f = do
    input <- getContents
    writeLines $ f $ lines input

writeLines :: [String] -> IO ()
writeLines outputs = forM_ outputs (\output -> do
        hFlush stdout
        putStrLn output
        putStr $ "\n"++prompt ":"
        hFlush stdout
    )

-- TODO: generate a default .ini file
