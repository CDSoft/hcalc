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

module Main(main) where

import Interface

import System.Environment

main :: IO ()
main = getArgs >>= doArgs
