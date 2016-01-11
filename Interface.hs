{-# LANGUAGE CPP #-}

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

module Interface(mainCalc) where

import Expression
import Parser
import PrettyPrint
import Help

--import Data.Ratio
--import Data.Bits
import Data.List
--import Data.Maybe
import System.Environment
--import Control.Monad

--import Foreign.C
--import Foreign.C.Types
--import Data.Word

#ifdef linux_HOST_OS
import Data.Char
import System.Console.Readline
import System.Exit
import System.Posix.Process
#endif
#ifdef mingw32_HOST_OS
import System.Process
--import System.IO
#endif

import System.IO
import System.FilePath
import System.Directory

--import Debug.Trace

mainCalc :: IO ()
mainCalc = do
    args <- getArgs
    doArgs args

doArgs :: [String] -> IO ()
doArgs [h] | h `elem` ["-l", "--license"] = doLicense
doArgs [h] | h `elem` ["-h", "--help"] = doHelp
doArgs [] = doREPL
doArgs exprs = doCLI $ intercalate "; " exprs

doLicense :: IO ()
doLicense = putStrLn welcome

doHelp :: IO ()
doHelp = do
    putStrLn welcome
    putStrLn ""
    putStrLn help

doCLI :: String -> IO ()
doCLI exprs = do
    --state <- initialState
    state <- loadIni emptyState
    let e = parse exprs
    print e
    let ((conf, _, _), v) = eval state e
    case v of
        Bye _ -> return ()
        _ -> putStrLn . (prompt "=" ++) . pp conf $ v

doREPL :: IO ()
doREPL = do
#ifdef mingw32_HOST_OS
    _ <- runCommand "title uCalc"
    _ <- runCommand "color f0"
#endif
#ifdef linux_HOST_OS
    nice 19
#endif
    putStrLn welcome
    -- state <- initialState
    state <- loadIni emptyState
    repl state Nop
    where
        readLine :: String -> IO String
        readLine inputPrompt = do
#ifdef linux_HOST_OS
            maybeLine <- readline inputPrompt
            case maybeLine of
                Nothing     -> exitSuccess
                Just line   -> if not $ null $ filter (not.isSpace) line then
                                    do  addHistory line
                                        return line
                               else
                                    do  return ""
#endif
#ifdef mingw32_HOST_OS
            putStr inputPrompt
            hFlush stdout
            line <- getLine
            return line
#endif
        repl :: State -> Expr -> IO ()
        repl st prev = do
            putStrLn ""
            line <- readLine (prompt ":")
            st' <- loadIni st
            let e = parse line
            let (st''@(conf, _, _), v) = eval st' e
            let prev' = if v == Previous then prev else v
            case v of
                Bye _ -> return ()
                _ -> do
                        putStrLn . (prompt "=" ++) . pp conf $ prev'
                        repl st'' prev'

iniName :: IO String
iniName = do
    path <- getExecutablePath
    return $ dropExtension path ++ ".ini"

{-
initialState :: IO State
initialState = do
    name <- iniName
    iniFileFound <- doesFileExist name
    if not iniFileFound
        then
            return emptyState
        else
            do
                putStrLn $ "Loading " ++ name
                h <- openFile name ReadMode
                hSetEncoding h utf8
                ini <- hGetContents h
                let (st, v) = eval emptyState $ parse ini
                case v of
                    E err -> putStrLn $ "Error while loading " ++ name ++ ": " ++ err
                    _ -> putStrLn $ name ++ " loaded."
                return st
-}

loadIni :: State -> IO State
loadIni st@(conf, _, _) = do
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
            putStrLn $ "Loading " ++ fName
            h <- openFile fName ReadMode
            hSetEncoding h utf8
            ini <- hGetContents h
            let (st', v) = eval st $ parse ini
            case v of
                E err -> putStrLn $ "Error while loading " ++ fName ++ ": " ++ err
                _ -> return ()
            return $ setMTime st' t

