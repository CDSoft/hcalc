module Main(main) where

import Expression
import Parser
import IEEE754
import ASCII
import Help
import Interface

import Data.Char
import Data.Ratio
--import Data.List
import qualified Data.Map as Map
import System.Environment
import System.Exit
import System.Process
import System.Directory
import System.FilePath
import Control.Monad
import Trace.Hpc.Reflect

data UnitTest = Parse String Expr Expr State
              | Eval String Expr State

data InteractiveTest = CLI [String] ExitCode String String
                     | REPL [String] ExitCode String String
                     | INI String
main :: IO ()
main = do
    exe <- getExecutablePath
    args <- getArgs
    case args of
        [] -> do
                doUnitTests
                doInteractiveTests
        "test":args' -> do
                clearTix
                doArgs args'
                writeTix interactiveTix
        _ -> error $ exe ++ " called with unexpected arguments"

doUnitTests :: IO ()
doUnitTests = do
    putStrLn "Starting unit tests"
    clearTix
    forM_ unitTests (\test -> case test of
            Parse s e v st -> do
                let e' = parse s
                let (st', v') = eval emptyState e
                when (e' /= e) $ error $ "Parser error: " ++ s ++ " should be parsed as " ++ show e ++ " but is " ++ show e'
                when (v' /== v) $ error $ "Evaluation error: " ++ s ++ " should be evaluated as " ++ show v ++ " but is " ++ show v'
                when (st' /= st) $ error $ "Evaluation error: " ++ s ++ " should lead to the state " ++ show st ++ " but leads to " ++ show st'
                --putStrLn $ s ++ " => " ++ show e ++ " => " ++ show v
            Eval s v st -> do
                let (st', v') = eval emptyState $ parse s
                when (v' /== v) $ error $ "Evaluation error: " ++ s ++ " should be evaluated as " ++ show v ++ " but is " ++ show v'
                when (st' /= st) $ error $ "Evaluation error: " ++ s ++ " should lead to the state " ++ show st ++ " but leads to " ++ show st'
                --putStrLn $ s ++ " => " ++ show v
        )
    writeTix "unit.tix"
    putStrLn $ "Unit tests passed (" ++ show (length unitTests) ++ " tests)"

(===) :: Expr -> Expr -> Bool
(R x) === (R y)
    | isNaN x && isNaN y = True -- right only for unit testing!
    | isNaN x = False
    | isNaN y = False
    | isInfinite x && isInfinite y = x*y > 0
    | isInfinite x = False
    | isInfinite y = False
    | abs x < eps && abs y < eps = True
    | otherwise = abs (x-y) / abs y < eps
    where eps = 1e-12
x === y = x == y

(/==) :: Expr -> Expr -> Bool
x /== y = not $ x === y

nan :: Double
nan = 0.0 / 0.0

inf :: Double
inf = 1.0 / 0.0

m :: [((String,Int), Expr)] -> Map.Map (String,Int) Expr
m = Map.fromList

unitTests :: [UnitTest]
unitTests =
    -- Parser tests
    [ Parse ":-)"       (E "Error near :-)")                        (E "Error near :-)") emptyState
    , Parse "# ..."     None                                        None emptyState
    , Parse "#\n#...\n" None                                        None emptyState
    , Parse "42"        (Z 42)                                      (Z 42) emptyState
    , Parse "0x42"      (Seq (SetHex Nothing) (Z 0x42))             (Z 0x42) (emptyConf{hex=True}, m[])
    , Parse "0x"        (E "Error near x")                          (E "Error near x") emptyState
    , Parse "0o42"      (Seq (SetOct Nothing) (Z 0o42))             (Z 0o42) (emptyConf{oct=True}, m[])
    , Parse "0o"        (E "Error near o")                          (E "Error near o") emptyState
    , Parse "0b110"     (Seq (SetBin Nothing) (Z 6))                (Z 6) (emptyConf{bin=True}, m[])
    , Parse "0b"        (E "Error near b")                          (E "Error near b") emptyState
    , Parse "42/6"      (Div (Z 42) (Z 6))                          (Z 7) emptyState
    , Parse "42/12"     (Div (Z 42) (Z 12))                         (Q (7%2)) emptyState
    , Parse "3.14"      (R 3.14)                                    (R 3.14) emptyState
    , Parse "0.0"       (R 0.0)                                     (R 0.0) emptyState
    , Parse "true"      (F "true" [])                               (B True) emptyState
    , Parse "false"     (F "false" [])                              (B False) emptyState
    , Parse "\"str\""   (S "str")                                   (S "str") emptyState
    , Parse "\"str\\\"\""   (S "str\"")                             (S "str\"") emptyState
    , Parse "\"str"     (S "str")                                   (S "str") emptyState
    , Parse "func"      (F "func" [])                               (E "'func' is not defined") emptyState
    , Parse "func(x)"   (F "func" [F "x" []])                       (E "'func'/1 is not defined") emptyState
    , Parse "func(x,1)" (F "func" [F "x" [], Z 1])                  (E "'func'/2 is not defined") emptyState
    , Parse "x=y"       (Def "x" [] (F "y" []))                     None (emptyConf, m[(("x",0), Def "x" [] (F "y" []))])
    , Parse "f(x)=y"    (Def "f" ["x"] (F "y" []))                  None (emptyConf, m[(("f",1), Def "f" ["x"] (F "y" []))])
    , Parse "f(x,y)=z"  (Def "f" ["x", "y"] (F "z" []))             None (emptyConf, m[(("f",2), Def "f" ["x","y"] (F "z" []))])
    , Parse ""          None                                        None emptyState
    , Parse "1"         (Z 1)                                       (Z 1) emptyState
    , Parse "1;"        (Z 1)                                       (Z 1) emptyState
    , Parse "1; 2"      (Seq (Z 1) (Z 2))                           (Z 2) emptyState
    , Parse "1; 2;"     (Seq (Z 1) (Z 2))                           (Z 2) emptyState
    , Parse "1; 2; 3"   (Seq (Seq (Z 1) (Z 2)) (Z 3))               (Z 3) emptyState
    , Parse "1; 2; 3;"  (Seq (Seq (Z 1) (Z 2)) (Z 3))               (Z 3) emptyState
    , Parse "c?t:f"     (Tern (F "c" []) (F "t" []) (F "f" []))     (E "'c' is not defined") emptyState
    ] ++
    -- evaluation
    [ Eval "f(x,y)=10*x+y; f(2,3)"      (Z 23) (emptyConf, m[(("f",2), Def "f" ["x","y"] (Add (Mul (Z 10) (F "x" [])) (F "y" [])))])

    , Eval "true?1:0"                   (Z 1) emptyState
    , Eval "false?1:0"                  (Z 0) emptyState
    , Eval "x?0:0"                      (E "'x' is not defined") emptyState
    , Eval "true?x:0"                   (E "'x' is not defined") emptyState
    , Eval "false?0:x"                  (E "'x' is not defined") emptyState
    , Eval "1?2:3"                      (E "non boolean condition") emptyState

    , Eval "x or 0"                     (E "'x' is not defined") emptyState
    , Eval "0 or x"                     (E "'x' is not defined") emptyState
    , Eval "1 or 2"                     (E "bad operands for 'or'") emptyState
    , Eval "false or 2"                 (E "bad operands for 'or'") emptyState
    , Eval "1 or false"                 (E "bad operands for 'or'") emptyState
    , Eval "false or false"             (B False) emptyState
    , Eval "false or true"              (B True) emptyState
    , Eval "true or false"              (B True) emptyState
    , Eval "true or true"               (B True) emptyState

    , Eval "x xor 0"                    (E "'x' is not defined") emptyState
    , Eval "0 xor x"                    (E "'x' is not defined") emptyState
    , Eval "1 xor 2"                    (E "bad operands for 'xor'") emptyState
    , Eval "false xor 2"                (E "bad operands for 'xor'") emptyState
    , Eval "1 xor false"                (E "bad operands for 'xor'") emptyState
    , Eval "false xor false"            (B False) emptyState
    , Eval "false xor true"             (B True) emptyState
    , Eval "true xor false"             (B True) emptyState
    , Eval "true xor true"              (B False) emptyState

    , Eval "x and 0"                    (E "'x' is not defined") emptyState
    , Eval "0 and x"                    (E "'x' is not defined") emptyState
    , Eval "1 and 2"                    (E "bad operands for 'and'") emptyState
    , Eval "false and 2"                (E "bad operands for 'and'") emptyState
    , Eval "1 and false"                (E "bad operands for 'and'") emptyState
    , Eval "false and false"            (B False) emptyState
    , Eval "false and true"             (B False) emptyState
    , Eval "true and false"             (B False) emptyState
    , Eval "true and true"              (B True) emptyState

    , Eval "not x"                      (E "'x' is not defined") emptyState
    , Eval "not 1"                      (E "bad operand for 'not'") emptyState
    , Eval "not true"                   (B False) emptyState
    , Eval "not false"                  (B True) emptyState

    , Eval "x<=0"                        (E "'x' is not defined") emptyState
    , Eval "0<=x"                        (E "'x' is not defined") emptyState
    , Eval "true <= 9"                  (E "bad operands for '<='") emptyState
    , Eval "9 <= true"                  (E "bad operands for '<='") emptyState
    , Eval "4 <= 5"                     (B True) emptyState
    , Eval "5 <= 5"                     (B True) emptyState
    , Eval "6 <= 5"                     (B False) emptyState
    , Eval "2 <= 5/2"                   (B True) emptyState
    , Eval "3 <= 5/2"                   (B False) emptyState
    , Eval "2 <= 2.5"                   (B True) emptyState
    , Eval "2 <= 2.0"                   (B True) emptyState
    , Eval "3 <= 2.5"                   (B False) emptyState
    , Eval "5/2 <= 3"                   (B True) emptyState
    , Eval "5/2 <= 2"                   (B False) emptyState
    , Eval "5/2 <= 5/2+1/10"            (B True) emptyState
    , Eval "5/2 <= 5/2+0/10"            (B True) emptyState
    , Eval "5/2 <= 5/2-1/10"            (B False) emptyState
    , Eval "5/2 <= 2.6"                 (B True) emptyState
    , Eval "5/2 <= 2.5"                 (B True) emptyState
    , Eval "5/2 <= 2.4"                 (B False) emptyState
    , Eval "2.5 <= 3"                   (B True) emptyState
    , Eval "3.0 <= 3"                   (B True) emptyState
    , Eval "2.5 <= 2"                   (B False) emptyState
    , Eval "2.5 <= 5/2+1/10"            (B True) emptyState
    , Eval "2.5 <= 5/2"                 (B True) emptyState
    , Eval "2.5 <= 5/2-1/10"            (B False) emptyState
    , Eval "2.5 <= 2.6"                 (B True) emptyState
    , Eval "2.5 <= 2.5"                 (B True) emptyState
    , Eval "2.5 <= 2.4"                 (B False) emptyState
    , Eval "\"ab\" <= \"ac\""           (B True) emptyState
    , Eval "\"ac\" <= \"ac\""           (B True) emptyState
    , Eval "\"ad\" <= \"ac\""           (B False) emptyState

    , Eval "x<0"                        (E "'x' is not defined") emptyState
    , Eval "0<x"                        (E "'x' is not defined") emptyState
    , Eval "true < 9"                   (E "bad operands for '<'") emptyState
    , Eval "9 < true"                   (E "bad operands for '<'") emptyState
    , Eval "4 < 5"                      (B True) emptyState
    , Eval "5 < 5"                      (B False) emptyState
    , Eval "6 < 5"                      (B False) emptyState
    , Eval "2 < 5/2"                    (B True) emptyState
    , Eval "3 < 5/2"                    (B False) emptyState
    , Eval "2 < 2.5"                    (B True) emptyState
    , Eval "2 < 2.0"                    (B False) emptyState
    , Eval "3 < 2.5"                    (B False) emptyState
    , Eval "5/2 < 3"                    (B True) emptyState
    , Eval "5/2 < 2"                    (B False) emptyState
    , Eval "5/2 < 5/2+1/10"             (B True) emptyState
    , Eval "5/2 < 5/2+0/10"             (B False) emptyState
    , Eval "5/2 < 5/2-1/10"             (B False) emptyState
    , Eval "5/2 < 2.6"                  (B True) emptyState
    , Eval "5/2 < 2.5"                  (B False) emptyState
    , Eval "5/2 < 2.4"                  (B False) emptyState
    , Eval "2.5 < 3"                    (B True) emptyState
    , Eval "3.0 < 3"                    (B False) emptyState
    , Eval "2.5 < 2"                    (B False) emptyState
    , Eval "2.5 < 5/2+1/10"             (B True) emptyState
    , Eval "2.5 < 5/2"                  (B False) emptyState
    , Eval "2.5 < 5/2-1/10"             (B False) emptyState
    , Eval "2.5 < 2.6"                  (B True) emptyState
    , Eval "2.5 < 2.5"                  (B False) emptyState
    , Eval "2.5 < 2.4"                  (B False) emptyState
    , Eval "\"ab\" < \"ac\""            (B True) emptyState
    , Eval "\"ac\" < \"ac\""            (B False) emptyState
    , Eval "\"ad\" < \"ac\""            (B False) emptyState

    , Eval "x>=0"                       (E "'x' is not defined") emptyState
    , Eval "0>=x"                       (E "'x' is not defined") emptyState
    , Eval "true >= 9"                  (E "bad operands for '>='") emptyState
    , Eval "9 >= true"                  (E "bad operands for '>='") emptyState
    , Eval "4 >= 5"                     (B False) emptyState
    , Eval "5 >= 5"                     (B True) emptyState
    , Eval "6 >= 5"                     (B True) emptyState
    , Eval "2 >= 5/2"                   (B False) emptyState
    , Eval "3 >= 5/2"                   (B True) emptyState
    , Eval "2 >= 2.5"                   (B False) emptyState
    , Eval "2 >= 2.0"                   (B True) emptyState
    , Eval "3 >= 2.5"                   (B True) emptyState
    , Eval "5/2 >= 3"                   (B False) emptyState
    , Eval "5/2 >= 2"                   (B True) emptyState
    , Eval "5/2 >= 5/2+1/10"            (B False) emptyState
    , Eval "5/2 >= 5/2+0/10"            (B True) emptyState
    , Eval "5/2 >= 5/2-1/10"            (B True) emptyState
    , Eval "5/2 >= 2.6"                 (B False) emptyState
    , Eval "5/2 >= 2.5"                 (B True) emptyState
    , Eval "5/2 >= 2.4"                 (B True) emptyState
    , Eval "2.5 >= 3"                   (B False) emptyState
    , Eval "3.0 >= 3"                   (B True) emptyState
    , Eval "2.5 >= 2"                   (B True) emptyState
    , Eval "2.5 >= 5/2+1/10"            (B False) emptyState
    , Eval "2.5 >= 5/2"                 (B True) emptyState
    , Eval "2.5 >= 5/2-1/10"            (B True) emptyState
    , Eval "2.5 >= 2.6"                 (B False) emptyState
    , Eval "2.5 >= 2.5"                 (B True) emptyState
    , Eval "2.5 >= 2.4"                 (B True) emptyState
    , Eval "\"ab\" >= \"ac\""           (B False) emptyState
    , Eval "\"ac\" >= \"ac\""           (B True) emptyState
    , Eval "\"ad\" >= \"ac\""           (B True) emptyState

    , Eval "x>0"                        (E "'x' is not defined") emptyState
    , Eval "0>x"                        (E "'x' is not defined") emptyState
    , Eval "true > 9"                   (E "bad operands for '>'") emptyState
    , Eval "9 > true"                   (E "bad operands for '>'") emptyState
    , Eval "4 > 5"                      (B False) emptyState
    , Eval "5 > 5"                      (B False) emptyState
    , Eval "6 > 5"                      (B True) emptyState
    , Eval "2 > 5/2"                    (B False) emptyState
    , Eval "3 > 5/2"                    (B True) emptyState
    , Eval "2 > 2.5"                    (B False) emptyState
    , Eval "2 > 2.0"                    (B False) emptyState
    , Eval "3 > 2.5"                    (B True) emptyState
    , Eval "5/2 > 3"                    (B False) emptyState
    , Eval "5/2 > 2"                    (B True) emptyState
    , Eval "5/2 > 5/2+1/10"             (B False) emptyState
    , Eval "5/2 > 5/2+0/10"             (B False) emptyState
    , Eval "5/2 > 5/2-1/10"             (B True) emptyState
    , Eval "5/2 > 2.6"                  (B False) emptyState
    , Eval "5/2 > 2.5"                  (B False) emptyState
    , Eval "5/2 > 2.4"                  (B True) emptyState
    , Eval "2.5 > 3"                    (B False) emptyState
    , Eval "3.0 > 3"                    (B False) emptyState
    , Eval "2.5 > 2"                    (B True) emptyState
    , Eval "2.5 > 5/2+1/10"             (B False) emptyState
    , Eval "2.5 > 5/2"                  (B False) emptyState
    , Eval "2.5 > 5/2-1/10"             (B True) emptyState
    , Eval "2.5 > 2.6"                  (B False) emptyState
    , Eval "2.5 > 2.5"                  (B False) emptyState
    , Eval "2.5 > 2.4"                  (B True) emptyState
    , Eval "\"ab\" > \"ac\""            (B False) emptyState
    , Eval "\"ac\" > \"ac\""            (B False) emptyState
    , Eval "\"ad\" > \"ac\""            (B True) emptyState

    , Eval "x==0"                       (E "'x' is not defined") emptyState
    , Eval "0==x"                       (E "'x' is not defined") emptyState
    , Eval "true == 9"                  (E "bad operands for '=='") emptyState
    , Eval "9 == true"                  (E "bad operands for '=='") emptyState
    , Eval "4 == 5"                     (B False) emptyState
    , Eval "5 == 5"                     (B True) emptyState
    , Eval "6 == 5"                     (B False) emptyState
    , Eval "2 == 5/2"                   (B False) emptyState
    , Eval "3 == 5/2"                   (B False) emptyState
    , Eval "2 == 2.5"                   (B False) emptyState
    , Eval "2 == 2.0"                   (B True) emptyState
    , Eval "3 == 2.5"                   (B False) emptyState
    , Eval "5/2 == 3"                   (B False) emptyState
    , Eval "5/2 == 2"                   (B False) emptyState
    , Eval "5/2 == 5/2+1/10"            (B False) emptyState
    , Eval "5/2 == 5/2+0/10"            (B True) emptyState
    , Eval "5/2 == 5/2-1/10"            (B False) emptyState
    , Eval "5/2 == 2.6"                 (B False) emptyState
    , Eval "5/2 == 2.5"                 (B True) emptyState
    , Eval "5/2 == 2.4"                 (B False) emptyState
    , Eval "2.5 == 3"                   (B False) emptyState
    , Eval "3.0 == 3"                   (B True) emptyState
    , Eval "2.5 == 2"                   (B False) emptyState
    , Eval "2.5 == 5/2+1/10"            (B False) emptyState
    , Eval "2.5 == 5/2"                 (B True) emptyState
    , Eval "2.5 == 5/2-1/10"            (B False) emptyState
    , Eval "2.5 == 2.6"                 (B False) emptyState
    , Eval "2.5 == 2.5"                 (B True) emptyState
    , Eval "2.5 == 2.4"                 (B False) emptyState
    , Eval "\"ab\" == \"ac\""           (B False) emptyState
    , Eval "\"ac\" == \"ac\""           (B True) emptyState
    , Eval "\"ad\" == \"ac\""           (B False) emptyState

    , Eval "x!=0"                       (E "'x' is not defined") emptyState
    , Eval "0!=x"                       (E "'x' is not defined") emptyState
    , Eval "true != 9"                  (E "bad operands for '!='") emptyState
    , Eval "9 != true"                  (E "bad operands for '!='") emptyState
    , Eval "4 != 5"                     (B True) emptyState
    , Eval "6 != 5"                     (B True) emptyState
    , Eval "5 != 5"                     (B False) emptyState
    , Eval "2 != 5/2"                   (B True) emptyState
    , Eval "3 != 5/2"                   (B True) emptyState
    , Eval "2 != 2.5"                   (B True) emptyState
    , Eval "2 != 2.0"                   (B False) emptyState
    , Eval "3 != 2.5"                   (B True) emptyState
    , Eval "5/2 != 3"                   (B True) emptyState
    , Eval "5/2 != 2"                   (B True) emptyState
    , Eval "5/2 != 5/2+1/10"            (B True) emptyState
    , Eval "5/2 != 5/2+0/10"            (B False) emptyState
    , Eval "5/2 != 5/2-1/10"            (B True) emptyState
    , Eval "5/2 != 2.6"                 (B True) emptyState
    , Eval "5/2 != 2.5"                 (B False) emptyState
    , Eval "5/2 != 2.4"                 (B True) emptyState
    , Eval "2.5 != 3"                   (B True) emptyState
    , Eval "3.0 != 3"                   (B False) emptyState
    , Eval "2.5 != 2"                   (B True) emptyState
    , Eval "2.5 != 5/2+1/10"            (B True) emptyState
    , Eval "2.5 != 5/2"                 (B False) emptyState
    , Eval "2.5 != 5/2-1/10"            (B True) emptyState
    , Eval "2.5 != 2.6"                 (B True) emptyState
    , Eval "2.5 != 2.5"                 (B False) emptyState
    , Eval "2.5 != 2.4"                 (B True) emptyState
    , Eval "\"ab\" != \"ac\""           (B True) emptyState
    , Eval "\"ac\" != \"ac\""           (B False) emptyState
    , Eval "\"ad\" != \"ac\""           (B True) emptyState

    , Eval "x|0"                        (E "'x' is not defined") emptyState
    , Eval "0|x"                        (E "'x' is not defined") emptyState
    , Eval "true|0"                     (E "bad operands for '|'") emptyState
    , Eval "0|true"                     (E "bad operands for '|'") emptyState
    , Eval "3|5"                        (Z 7) (emptyConf{hex=True}, m[])

    , Eval "x^0"                        (E "'x' is not defined") emptyState
    , Eval "0^x"                        (E "'x' is not defined") emptyState
    , Eval "true^0"                     (E "bad operands for '^'") emptyState
    , Eval "0^true"                     (E "bad operands for '^'") emptyState
    , Eval "3^5"                        (Z 6) (emptyConf{hex=True}, m[])

    , Eval "x&0"                        (E "'x' is not defined") emptyState
    , Eval "0&x"                        (E "'x' is not defined") emptyState
    , Eval "true&0"                     (E "bad operands for '&'") emptyState
    , Eval "0&true"                     (E "bad operands for '&'") emptyState
    , Eval "3&5"                        (Z 1) (emptyConf{hex=True}, m[])

    , Eval "x<<0"                       (E "'x' is not defined") emptyState
    , Eval "0<<x"                       (E "'x' is not defined") emptyState
    , Eval "true<<0"                    (E "bad operands for '<<'") emptyState
    , Eval "0<<true"                    (E "bad operands for '<<'") emptyState
    , Eval "3<<5"                       (Z (3*2^(5::Integer))) (emptyConf{hex=True}, m[])
    , Eval "3<<0"                       (Z 3) (emptyConf{hex=True}, m[])
    , Eval "3<<(-1)"                    (Z 1) (emptyConf{hex=True}, m[])
    , Eval "3<<(-2)"                    (Z 0) (emptyConf{hex=True}, m[])

    , Eval "x>>0"                       (E "'x' is not defined") emptyState
    , Eval "0>>x"                       (E "'x' is not defined") emptyState
    , Eval "true>>0"                    (E "bad operands for '>>'") emptyState
    , Eval "0>>true"                    (E "bad operands for '>>'") emptyState
    , Eval "3>>5"                       (Z 0) (emptyConf{hex=True}, m[])
    , Eval "3>>2"                       (Z 0) (emptyConf{hex=True}, m[])
    , Eval "3>>1"                       (Z 1) (emptyConf{hex=True}, m[])
    , Eval "3>>0"                       (Z 3) (emptyConf{hex=True}, m[])
    , Eval "3>>(-2)"                    (Z (3*2^(2::Integer))) (emptyConf{hex=True}, m[])

    , Eval "x+0"                        (E "'x' is not defined") emptyState
    , Eval "0+x"                        (E "'x' is not defined") emptyState
    , Eval "true + 9"                   (E "bad operands for '+'") emptyState
    , Eval "9 + true"                   (E "bad operands for '+'") emptyState
    , Eval "1+2"                        (Z 3) emptyState
    , Eval "1+2/3"                      (Q (5%3)) emptyState
    , Eval "1+2.5"                      (R 3.5) emptyState
    , Eval "1/2+3"                      (Q (7%2)) emptyState
    , Eval "1/2+3/4"                    (Q (5%4)) emptyState
    , Eval "1/2+3.0"                    (R 3.5) emptyState
    , Eval "1.5+1"                      (R 2.5) emptyState
    , Eval "1.5+1/2"                    (R 2.0) emptyState
    , Eval "1.5+2.75"                   (R 4.25) emptyState
    , Eval "\"str\"+42"                 (S "str42") emptyState
    , Eval "\"str\"+42/5"               (S "str42/5") emptyState
    , Eval "\"str\"+42.5"               (S "str42.5") emptyState
    , Eval "42+\"str\""                 (S "42str") emptyState
    , Eval "42/5+\"str\""               (S "42/5str") emptyState
    , Eval "42.5+\"str\""               (S "42.5str") emptyState
    , Eval "\"x\"+\"y\""                (S "xy") emptyState
    , Eval "\"x\"+true"                 (S "xtrue") emptyState
    , Eval "\"x\"+false"                (S "xfalse") emptyState
    , Eval "\"x\"+(x=y)"                (S "x") (emptyConf, m[(("x",0), Def "x" [] (F "y" []))])

    , Eval "x-0"                        (E "'x' is not defined") emptyState
    , Eval "0-x"                        (E "'x' is not defined") emptyState
    , Eval "true - 9"                   (E "bad operands for '-'") emptyState
    , Eval "9 - true"                   (E "bad operands for '-'") emptyState
    , Eval "1-2"                        (Z (-1)) emptyState
    , Eval "1-2/3"                      (Q (1%3)) emptyState
    , Eval "1-2.5"                      (R (-1.5)) emptyState
    , Eval "1/2-3"                      (Q (-5%2)) emptyState
    , Eval "1/2-3/4"                    (Q (-1%4)) emptyState
    , Eval "1/2-3.0"                    (R (-2.5)) emptyState
    , Eval "1.5-1"                      (R 0.5) emptyState
    , Eval "1.5-1/2"                    (R 1.0) emptyState
    , Eval "1.5-2.75"                   (R (-1.25)) emptyState

    , Eval "x*0"                        (E "'x' is not defined") emptyState
    , Eval "0*x"                        (E "'x' is not defined") emptyState
    , Eval "true * 9"                   (E "bad operands for '*'") emptyState
    , Eval "9 * true"                   (E "bad operands for '*'") emptyState
    , Eval "2*3"                        (Z 6) emptyState
    , Eval "2*(3/4)"                    (Q (3%2)) emptyState
    , Eval "2*3.5"                      (R 7) emptyState
    , Eval "2/3*4"                      (Q (8%3)) emptyState
    , Eval "2/3*(4/5)"                  (Q (8%15)) emptyState
    , Eval "3/4*5.5"                    (R 4.125) emptyState
    , Eval "2.5*3"                      (R 7.5) emptyState
    , Eval "2.5*(3/4)"                  (R 1.875) emptyState
    , Eval "2.5*3.5"                    (R 8.75) emptyState
    , Eval "\"ab\"*3"                   (S "ababab") emptyState
    , Eval "3*\"ab\""                   (S "ababab") emptyState

    , Eval "x/0"                        (E "'x' is not defined") emptyState
    , Eval "0/x"                        (E "'x' is not defined") emptyState
    , Eval "true / 9"                   (E "bad operands for '/'") emptyState
    , Eval "9 / true"                   (E "bad operands for '/'") emptyState
    , Eval "2/3"                        (Q (2%3)) emptyState
    , Eval "2/(3/4)"                    (Q (8%3)) emptyState
    , Eval "2/3.5"                      (R (2/3.5)) emptyState
    , Eval "(2/3)/4"                    (Q (1%6)) emptyState
    , Eval "(2/3)/(4/5)"                (Q (5%6)) emptyState
    , Eval "(3/4)/5.5"                  (R (0.75/5.5)) emptyState
    , Eval "2.5/3"                      (R (2.5/3)) emptyState
    , Eval "2.5/(3/4)"                  (R (2.5/0.75)) emptyState
    , Eval "2.5/3.5"                    (R (2.5/3.5)) emptyState
    , Eval "1/0"                        (E "Zero division") emptyState
    , Eval "(1/2)/0"                    (E "Zero division") emptyState
    , Eval "0.0/0"                      (R nan) emptyState
    , Eval "1.0/0"                      (R (1.0/0)) emptyState
    , Eval "(-1.0)/0"                   (R (-1.0/0)) emptyState

    , Eval "x//0"                       (E "'x' is not defined") emptyState
    , Eval "0//x"                       (E "'x' is not defined") emptyState
    , Eval "true // 9"                  (E "bad operands for '//'") emptyState
    , Eval "9 // true"                  (E "bad operands for '//'") emptyState
    , Eval "7//0"                       (E "Zero division") emptyState
    , Eval "66//9"                      (Z 7) emptyState
    , Eval "(-66)//9"                   (Z (-8)) emptyState
    , Eval "0//9"                       (Z 0) emptyState

    , Eval "x%0"                        (E "'x' is not defined") emptyState
    , Eval "0%x"                        (E "'x' is not defined") emptyState
    , Eval "true % 9"                   (E "bad operands for '%'") emptyState
    , Eval "9 % true"                   (E "bad operands for '%'") emptyState
    , Eval "7%0"                        (E "Zero division") emptyState
    , Eval "66%9"                       (Z 3) emptyState
    , Eval "(-66)%9"                    (Z 6) emptyState
    , Eval "0%9"                        (Z 0) emptyState

    , Eval "+x"                         (E "'x' is not defined") emptyState
    , Eval "+true"                      (E "bad operand for '+'") emptyState
    , Eval "+5"                         (Z 5) emptyState
    , Eval "+(-5)"                      (Z (-5)) emptyState
    , Eval "+(5/2)"                     (Q (5%2)) emptyState
    , Eval "+(-5/2)"                    (Q (-5%2)) emptyState
    , Eval "+5.5"                       (R 5.5) emptyState
    , Eval "+(-5.5)"                    (R (-5.5)) emptyState

    , Eval "-x"                         (E "'x' is not defined") emptyState
    , Eval "-true"                      (E "bad operand for '-'") emptyState
    , Eval "-5"                         (Z (-5)) emptyState
    , Eval "-(-5)"                      (Z 5) emptyState
    , Eval "-(5/2)"                     (Q (-5%2)) emptyState
    , Eval "-(-5/2)"                    (Q (5%2)) emptyState
    , Eval "-5.5"                       (R (-5.5)) emptyState
    , Eval "-(-5.5)"                    (R 5.5) emptyState

    , Eval "~x"                         (E "'x' is not defined") emptyState
    , Eval "~true"                      (E "bad operand for '~'") emptyState
    , Eval "~5"                         (Z (-6)) (emptyConf{hex=True}, m[])

    , Eval "x**0"                       (E "'x' is not defined") emptyState
    , Eval "0**x"                       (E "'x' is not defined") emptyState
    , Eval "true ** 9"                  (E "bad operands for '**'") emptyState
    , Eval "9 ** true"                  (E "bad operands for '**'") emptyState
    , Eval "2**3"                       (Z 8) emptyState
    , Eval "2**(3/4)"                   (R (2**0.75)) emptyState
    , Eval "2**3.5"                     (R (2**3.5)) emptyState
    , Eval "(2/3)**4"                   (Q (16%81)) emptyState
    , Eval "(2/3)**(4/5)"               (R ((2/3)**(4/5))) emptyState
    , Eval "(3/4)**5.5"                 (R (0.75**5.5)) emptyState
    , Eval "2.5**3"                     (R (2.5**3)) emptyState
    , Eval "2.5**(3/4)"                 (R (2.5**0.75)) emptyState
    , Eval "2.5**3.5"                   (R (2.5**3.5)) emptyState
    ] ++
    -- Configuration
    [ Eval "hex32; oct32; bin32; dec32; float32; reset"     Previous emptyState
    , Eval "hex"                        Previous (emptyConf{hex=True}, m[])
    , Eval "oct"                        Previous (emptyConf{oct=True}, m[])
    , Eval "bin"                        Previous (emptyConf{bin=True}, m[])
    , Eval "dec"                        Previous (emptyConf{dec=True}, m[])
    , Eval "float"                      Previous (emptyConf{float=True}, m[])
    , Eval "hex8"                       Previous (emptyConf{hex=True, size=8}, m[])
    , Eval "oct16"                      Previous (emptyConf{oct=True, size=16}, m[])
    , Eval "bin32"                      Previous (emptyConf{bin=True, size=32}, m[])
    , Eval "dec64"                      Previous (emptyConf{dec=True, size=64}, m[])
    , Eval "float32"                    Previous (emptyConf{float=True, size=32}, m[])
    , Eval "float64"                    Previous (emptyConf{float=True, size=64}, m[])
    ] ++
    -- builtin definitions
    [ Eval "true"                       (B True) emptyState
    , Eval "false"                      (B False) emptyState
    , Eval "pi"                         (R pi) emptyState
    , Eval "e"                          (R $ exp 1) emptyState
    , Eval "nan"                        (R nan) emptyState
    , Eval "inf"                        (R inf) emptyState

    , Eval "int(x)"                     (E "'x' is not defined") emptyState
    , Eval "int(true)"                  (E "bad operand for int") emptyState
    , Eval "int(3)"                     (Z 3) emptyState
    , Eval "int(-3)"                    (Z $ -3) emptyState
    , Eval "int(13/4)"                  (Z 3) emptyState
    , Eval "int(15/4)"                  (Z 3) emptyState
    , Eval "int(-13/4)"                 (Z $ -4) emptyState
    , Eval "int(-15/4)"                 (Z $ -4) emptyState

    , Eval "int(3.25)"                  (Z 3) emptyState
    , Eval "int(3.75)"                  (Z 3) emptyState
    , Eval "int(-3.25)"                 (Z $ -4) emptyState
    , Eval "int(-3.75)"                 (Z $ -4) emptyState

    , Eval "float(x)"                   (E "'x' is not defined") emptyState
    , Eval "float(true)"                (E "bad operand for float") emptyState
    , Eval "float(3)"                   (R 3) emptyState
    , Eval "float(3/2)"                 (R 1.5) emptyState
    , Eval "float(3.5)"                 (R 3.5) emptyState

    , Eval "rat(x)"                     (E "'x' is not defined") emptyState
    , Eval "rat(true)"                  (E "bad operand for rat") emptyState
    , Eval "rat(3)"                     (Z 3) emptyState
    , Eval "rat(2/7)"                   (Q (2%7)) emptyState
    , Eval "rat(1.5)"                   (Q (3%2)) emptyState

    , Eval "rat(x, 0)"                  (E "'x' is not defined") emptyState
    , Eval "rat(0, x)"                  (E "'x' is not defined") emptyState
    , Eval "rat(true, 0)"               (E "bad operands for rat") emptyState
    , Eval "rat(0, true)"               (E "bad operands for rat") emptyState
    , Eval "rat(3, 1)"                  (Z 3) emptyState
    , Eval "rat(3, 1/100)"              (Z 3) emptyState
    , Eval "rat(3, 0.100)"              (Z 3) emptyState
    , Eval "rat(3/2, 1)"                (Q (3%2)) emptyState
    , Eval "rat(3/2, 1/100)"            (Q (3%2)) emptyState
    , Eval "rat(3/2, 0.100)"            (Q (3%2)) emptyState
    , Eval "rat(pi, 1)"                 (Z 3) emptyState
    , Eval "rat(pi, 1/100)"             (Q (22%7)) emptyState
    , Eval "rat(pi, 0.01)"              (Q (22%7)) emptyState
    , Eval "rat(pi, 0)"                 (Q (884279719003555 % 281474976710656)) emptyState

    , Eval "abs(x)"                     (E "'x' is not defined") emptyState
    , Eval "abs(true)"                  (E "bad operand for abs") emptyState
    , Eval "abs(1)"                     (Z 1) emptyState
    , Eval "abs(-1)"                    (Z 1) emptyState
    , Eval "abs(1/2)"                   (Q $ 1%2) emptyState
    , Eval "abs(-1/2)"                  (Q $ 1%2) emptyState
    , Eval "abs(1.5)"                   (R 1.5) emptyState
    , Eval "abs(-1.5)"                  (R 1.5) emptyState

    , Eval "ceil(3)"                    (Z 3) emptyState
    , Eval "ceil(-3)"                   (Z $ -3) emptyState
    , Eval "ceil(3+1/4)"                (Z 4) emptyState
    , Eval "ceil(3+2/4)"                (Z 4) emptyState
    , Eval "ceil(3+3/4)"                (Z 4) emptyState
    , Eval "ceil(-3-1/4)"               (Z $ -3) emptyState
    , Eval "ceil(-3-2/4)"               (Z $ -3) emptyState
    , Eval "ceil(-3-3/4)"               (Z $ -3) emptyState
    , Eval "ceil(3.25)"                 (Z 4) emptyState
    , Eval "ceil(3.50)"                 (Z 4) emptyState
    , Eval "ceil(3.75)"                 (Z 4) emptyState
    , Eval "ceil(-3.25)"                (Z $ -3) emptyState
    , Eval "ceil(-3.50)"                (Z $ -3) emptyState
    , Eval "ceil(-3.75)"                (Z $ -3) emptyState

    , Eval "floor(3)"                   (Z 3) emptyState
    , Eval "floor(-3)"                  (Z $ -3) emptyState
    , Eval "floor(3+1/4)"               (Z 3) emptyState
    , Eval "floor(3+2/4)"               (Z 3) emptyState
    , Eval "floor(3+3/4)"               (Z 3) emptyState
    , Eval "floor(-3-1/4)"              (Z $ -4) emptyState
    , Eval "floor(-3-2/4)"              (Z $ -4) emptyState
    , Eval "floor(-3-3/4)"              (Z $ -4) emptyState
    , Eval "floor(3.25)"                (Z 3) emptyState
    , Eval "floor(3.50)"                (Z 3) emptyState
    , Eval "floor(3.75)"                (Z 3) emptyState
    , Eval "floor(-3.25)"               (Z $ -4) emptyState
    , Eval "floor(-3.50)"               (Z $ -4) emptyState
    , Eval "floor(-3.75)"               (Z $ -4) emptyState

    , Eval "round(3)"                   (Z 3) emptyState
    , Eval "round(-3)"                  (Z $ -3) emptyState
    , Eval "round(3+1/4)"               (Z 3) emptyState
    , Eval "round(3+2/4)"               (Z 4) emptyState
    , Eval "round(3+3/4)"               (Z 4) emptyState
    , Eval "round(-3-1/4)"              (Z $ -3) emptyState
    , Eval "round(-3-2/4)"              (Z $ -4) emptyState
    , Eval "round(-3-3/4)"              (Z $ -4) emptyState
    , Eval "round(3.25)"                (Z 3) emptyState
    , Eval "round(3.50)"                (Z 4) emptyState
    , Eval "round(3.75)"                (Z 4) emptyState
    , Eval "round(-3.25)"               (Z $ -3) emptyState
    , Eval "round(-3.50)"               (Z $ -4) emptyState
    , Eval "round(-3.75)"               (Z $ -4) emptyState

    , Eval "trunc(3)"                   (Z 3) emptyState
    , Eval "trunc(-3)"                  (Z $ -3) emptyState
    , Eval "trunc(3+1/4)"               (Z 3) emptyState
    , Eval "trunc(3+2/4)"               (Z 3) emptyState
    , Eval "trunc(3+3/4)"               (Z 3) emptyState
    , Eval "trunc(-3-1/4)"              (Z $ -3) emptyState
    , Eval "trunc(-3-2/4)"              (Z $ -3) emptyState
    , Eval "trunc(-3-3/4)"              (Z $ -3) emptyState
    , Eval "trunc(3.25)"                (Z 3) emptyState
    , Eval "trunc(3.50)"                (Z 3) emptyState
    , Eval "trunc(3.75)"                (Z 3) emptyState
    , Eval "trunc(-3.25)"               (Z $ -3) emptyState
    , Eval "trunc(-3.50)"               (Z $ -3) emptyState
    , Eval "trunc(-3.75)"               (Z $ -3) emptyState

    , Eval "mantissa(24)"               (R 0.75) emptyState
    , Eval "mantissa(3/4/2**12)"        (R 0.75) emptyState
    , Eval "mantissa(0.75*2**-12)"      (R 0.75) emptyState

    , Eval "exponent(24)"               (Z 5) emptyState
    , Eval "exponent(3/4/2**12)"        (Z (-12)) emptyState
    , Eval "exponent(0.75*2**-12)"      (Z (-12)) emptyState

    , Eval "fract(3.75)"                (R 0.75) emptyState
    , Eval "fract(-3.75)"               (R $ -0.75) emptyState

    , Eval "min(x, 0)"                  (E "'x' is not defined") emptyState
    , Eval "min(0, x)"                  (E "'x' is not defined") emptyState
    , Eval "min(true, 0)"               (E "bad operands for min") emptyState
    , Eval "min(0, true)"               (E "bad operands for min") emptyState
    , Eval "min(1, 2)"                  (Z 1) emptyState
    , Eval "min(2, 1)"                  (Z 1) emptyState
    , Eval "min(1, 2/3)"                (Q (2%3)) emptyState
    , Eval "min(2/3, 1)"                (Q (2%3)) emptyState
    , Eval "min(1, 2.5)"                (R 1) emptyState
    , Eval "min(2.5, 1)"                (R 1) emptyState
    , Eval "min(1/3, 2/3)"              (Q $ 1%3) emptyState
    , Eval "min(2/3, 1/3)"              (Q $ 1%3) emptyState
    , Eval "min(1.5, 2/5)"              (R 0.4) emptyState
    , Eval "min(2/5, 1.5)"              (R 0.4) emptyState
    , Eval "min(1.0, 2.0)"              (R 1) emptyState
    , Eval "min(2.0, 1.0)"              (R 1) emptyState

    , Eval "max(1, 2)"                  (Z 2) emptyState
    , Eval "max(2, 1)"                  (Z 2) emptyState
    , Eval "max(1/3, 2/3)"              (Q $ 2%3) emptyState
    , Eval "max(2/3, 1/3)"              (Q $ 2%3) emptyState
    , Eval "max(1.0, 2.0)"              (R 2) emptyState
    , Eval "max(2.0, 1.0)"              (R 2) emptyState

    , Eval "sqr(3)"                     (Z 9) emptyState
    , Eval "sqr(3/2)"                   (Q $ 9%4) emptyState
    , Eval "sqr(3.5)"                   (R $ 3.5*3.5) emptyState

    , Eval "sqrt(9)"                    (R 3) emptyState
    , Eval "sqrt(9/4)"                  (R 1.5) emptyState
    , Eval "sqrt(3.5*3.5)"              (R 3.5) emptyState
    , Eval "sqrt(-1)"                   (R nan) emptyState

    , Eval "cbrt(27)"                   (R 3) emptyState
    , Eval "cbrt(27/8)"                 (R 1.5) emptyState
    , Eval "cbrt(27.0/8.0)"             (R 1.5) emptyState

    , Eval "cos(pi/6)"                  (R (sqrt 3 / 2)) emptyState
    , Eval "cos(pi/4)"                  (R (sqrt 2 / 2)) emptyState
    , Eval "cos(pi/3)"                  (R 0.5) emptyState

    , Eval "sin(pi/3)"                  (R (sqrt 3 / 2)) emptyState
    , Eval "sin(pi/4)"                  (R (sqrt 2 / 2)) emptyState
    , Eval "sin(pi/6)"                  (R 0.5) emptyState

    , Eval "tan(pi/6)"                  (R (sqrt 3 / 3)) emptyState
    , Eval "tan(pi/4)"                  (R 1) emptyState
    , Eval "tan(pi/3)"                  (R (sqrt 3)) emptyState

    , Eval "acos(sqrt(3)/2)"            (R (pi/6)) emptyState
    , Eval "acos(sqrt(2)/2)"            (R (pi/4)) emptyState
    , Eval "acos(1/2)"                  (R (pi/3)) emptyState

    , Eval "asin(sqrt(3)/2)"            (R (pi/3)) emptyState
    , Eval "asin(sqrt(2)/2)"            (R (pi/4)) emptyState
    , Eval "asin(1/2)"                  (R (pi/6)) emptyState

    , Eval "atan(sqrt(3)/3)"            (R (pi/6)) emptyState
    , Eval "atan(1)"                    (R (pi/4)) emptyState
    , Eval "atan(sqrt(3))"              (R (pi/3)) emptyState

    , Eval "cosh(0)"                    (R 1) emptyState
    , Eval "cosh(1)"                    (R $ (exp 2+1)/(2*exp 1)) emptyState

    , Eval "sinh(0)"                    (R 0) emptyState
    , Eval "sinh(1)"                    (R $ (exp 2-1)/(2*exp 1)) emptyState

    , Eval "tanh(0)"                    (R 0) emptyState
    , Eval "tanh(1)"                    (R $ (exp 2-1)/(exp 2+1)) emptyState

    , Eval "atan(x, 0)"                 (E "'x' is not defined") emptyState
    , Eval "atan(0, x)"                 (E "'x' is not defined") emptyState
    , Eval "atan(true, 0)"              (E "bad operands for atan") emptyState
    , Eval "atan(0, true)"              (E "bad operands for atan") emptyState

    , Eval "atan(sqrt(3), 3)"           (R (pi/6)) emptyState
    , Eval "atan(1, 1)"                 (R (pi/4)) emptyState
    , Eval "atan(sqrt(3), 1)"           (R (pi/3)) emptyState

    , Eval "atan2(sqrt(3), 3)"          (R (pi/6)) emptyState
    , Eval "atan2(1, 1)"                (R (pi/4)) emptyState
    , Eval "atan2(sqrt(3), 1)"          (R (pi/3)) emptyState

    , Eval "atan(1, 1/2)"               (R (atan2 1 0.5)) emptyState
    , Eval "atan(1, 0.5)"               (R (atan2 1 0.5)) emptyState
    , Eval "atan(1/2, 1)"               (R (atan2 0.5 1)) emptyState
    , Eval "atan(1/2, 3/2)"             (R (atan2 0.5 1.5)) emptyState
    , Eval "atan(1/2, 1.5)"             (R (atan2 0.5 1.5)) emptyState
    , Eval "atan(0.5, 3/2)"             (R (atan2 0.5 1.5)) emptyState

    , Eval "deg(0)"                     (R 0) emptyState
    , Eval "deg(pi/6)"                  (R 30) emptyState
    , Eval "deg(pi/4)"                  (R 45) emptyState
    , Eval "deg(pi/3)"                  (R 60) emptyState

    , Eval "rad(0)"                     (R 0) emptyState
    , Eval "rad(30)"                    (R (pi/6)) emptyState
    , Eval "rad(45)"                    (R (pi/4)) emptyState
    , Eval "rad(60)"                    (R (pi/3)) emptyState

    , Eval "exp(0)"                     (R 1) emptyState
    , Eval "exp(1)"                     (R (exp 1)) emptyState
    , Eval "exp(3)"                     (R (exp 3)) emptyState

    , Eval "log(0)"                     (R (-inf)) emptyState
    , Eval "log(e)"                     (R 1) emptyState
    , Eval "log(e**3)"                  (R 3) emptyState

    , Eval "ln(0)"                      (R (-inf)) emptyState
    , Eval "ln(e)"                      (R 1) emptyState
    , Eval "ln(e**3)"                   (R 3) emptyState

    , Eval "log10(0)"                   (R (-inf)) emptyState
    , Eval "log10(10)"                  (R 1) emptyState
    , Eval "log10(10**3)"               (R 3) emptyState

    , Eval "log2(0)"                    (R (-inf)) emptyState
    , Eval "log2(2)"                    (R 1) emptyState
    , Eval "log2(2**3)"                 (R 3) emptyState

    , Eval "log(pi, 0)"                 (R (-inf)) emptyState
    , Eval "log(pi, pi)"                (R 1) emptyState
    , Eval "log(pi, pi**3)"             (R 3) emptyState

    , Eval "float2ieee(x)"              (E "'x' is not defined") emptyState
    , Eval "float2ieee(true)"           (E "bad operand for float2ieee") emptyState
    , Eval "float2ieee(24)"             (Z 0x41C00000) emptyState
    , Eval "float2ieee(1/2)"            (Z 0x3F000000) emptyState
    , Eval "float2ieee(pi)"             (Z 0x40490FDB) emptyState
    , Eval "ieee2float(x)"              (E "'x' is not defined") emptyState
    , Eval "ieee2float(true)"           (E "bad operand for ieee2float") emptyState
    , Eval "ieee2float(1078530011)"     (R $ floatToDouble $ doubleToFloat pi) emptyState

    , Eval "double2ieee(24)"            (Z 0x4038000000000000) emptyState
    , Eval "double2ieee(1/2)"           (Z 0x3FE0000000000000) emptyState
    , Eval "double2ieee(pi)"            (Z 0x400921FB54442D18) emptyState
    , Eval "ieee2double(4614256656552045848)"   (R pi) emptyState

    , Eval "isfinite(x)"                (E "'x' is not defined") emptyState
    , Eval "isfinite(true)"             (E "bad operand for isfinite") emptyState
    , Eval "isfinite(42<<10)"           (B True) (emptyConf{hex=True}, m[])
    , Eval "isfinite(42<<10000)"        (B False) (emptyConf{hex=True}, m[])
    , Eval "isfinite((42<<10)/5)"       (B True) (emptyConf{hex=True}, m[])
    , Eval "isfinite((42<<10000)/5)"    (B False) (emptyConf{hex=True}, m[])
    , Eval "isfinite(pi)"               (B True) emptyState
    , Eval "isfinite(inf)"              (B False) emptyState
    , Eval "isfinite(-inf)"             (B False) emptyState
    , Eval "isfinite(nan)"              (B False) emptyState

    , Eval "isinf(pi)"                  (B False) emptyState
    , Eval "isinf(inf)"                 (B True) emptyState
    , Eval "isinf(-inf)"                (B True) emptyState
    , Eval "isinf(nan)"                 (B False) emptyState

    , Eval "isnan(pi)"                  (B False) emptyState
    , Eval "isnan(inf)"                 (B False) emptyState
    , Eval "isnan(-inf)"                (B False) emptyState
    , Eval "isnan(nan)"                 (B True) emptyState
    ] ++
    -- commands
    [ Eval "ascii"                      (Put "ascii" $ unlines ["", ascii]) emptyState
    , Eval "help"                       (Put "help" $ unlines ["", shortHelp, "", longHelp]) emptyState
    , Eval "bye"                        (Bye "bye") emptyState
    , Eval "exit"                       (Bye "exit") emptyState
    ]

doInteractiveTests :: IO ()
doInteractiveTests = do
    exe <- getExecutablePath
    putStrLn "Starting interactive tests"
    forM_ interactions (\test -> case test of
            CLI args exitcode stdout stderr -> do
                exists <- keepCurrentTix
                (exitcode', stdout', stderr') <- readProcessWithExitCode exe ("test":args) ""
                when (exitcode' /= exitcode) $ error $ exe ++ " " ++ unwords args ++ " exit code is " ++ show exitcode' ++ " instead of " ++ show exitcode
                when (s stdout' /= s stdout) $ error $ exe ++ " " ++ unwords args ++ " prints " ++ stdout' ++ " instead of " ++ stdout
                when (s stderr' /= s stderr) $ error $ exe ++ " " ++ unwords args ++ " prints " ++ stderr' ++ " instead of " ++ stderr
                combineTix exists
            REPL stdin exitcode stdout stderr -> do
                exists <- keepCurrentTix
                (exitcode', stdout', stderr') <- readProcessWithExitCode exe ["test"] (unlines stdin ++ "bye")
                when (exitcode' /= exitcode) $ error $ exe ++ " < " ++ unwords stdin ++ " exit code is " ++ show exitcode' ++ " instead of " ++ show exitcode
                when (s stdout' /= s stdout) $ error $ exe ++ " < " ++ unwords stdin ++ " prints " ++ stdout' ++ " instead of " ++ stdout
                when (s stderr' /= s stderr) $ error $ exe ++ " < " ++ unwords stdin ++ " prints " ++ stderr' ++ " instead of " ++ stderr
                combineTix exists
            INI content -> do
                let ini = replaceExtension exe ".ini"
                exists <- doesFileExist ini
                case content of
                    "" -> when exists $ removeFile ini
                    _ -> writeFile ini content
        )
    putStrLn $ "Interactive tests passed (" ++ show (length $ filter isTest interactions) ++ " tests)"

    where
        s = filter (\c -> c /= '\n' && not (isSpace c))

        isTest (INI _) = False
        isTest _ = True

        keepCurrentTix :: IO Bool
        keepCurrentTix = do
            exists <- doesFileExist interactiveTix
            when exists $ renameFile interactiveTix currentTix
            return exists

        combineTix :: Bool -> IO ()
        combineTix exists = when exists $ do
            callProcess "hpc" ["combine", "--exclude=Main", "--union", "--output", nextTix, currentTix, interactiveTix]
            removeFile currentTix
            renameFile nextTix interactiveTix

        nextTix :: FilePath
        nextTix = "next.tix"

        currentTix :: FilePath
        currentTix = "current.tix"

interactiveTix :: FilePath
interactiveTix = "interactive.tix"

writeTix :: FilePath -> IO ()
writeTix name = do
    tix <- examineTix
    writeFile name (show tix)

interactions :: [InteractiveTest]
interactions =
    [ INI "" -- start tests with no .ini file
    , CLI ["help"] ExitSuccess (shortHelp++longHelp) ""
    , CLI ["license"] ExitSuccess license ""
    , CLI ["bye"] ExitSuccess "" ""
    , CLI ["2*21"] ExitSuccess "=  42" ""
    , CLI ["f(x)=42*x", "f(101)"] ExitSuccess "= 4242" ""
    , REPL ["bye"] ExitSuccess (shortHelp++": bye") ""
    , REPL ["1+1", "bye"] ExitSuccess (shortHelp++": 1+1\n= 2\n: bye") ""
    , REPL ["hex", "-1", "reset", "hex8", "bye"] ExitSuccess
           (unlines [ shortHelp
                    , ": hex"
                    , ": -1"
                    , "= -1"
                    , "hex -0x1"
                    , ": reset"
                    , "= -1"
                    , ": hex8"
                    , "= -1"
                    , "hex8 0xff"
                    , ": bye"
                    ]) ""
    , REPL ["  1  ", "  ", "  2  "] ExitSuccess
           (unlines [ shortHelp
                    , ": 1", "= 1"
                    , ": "
                    , ": 2", "= 2"
                    , ": bye"
                    ]) ""
    , INI ""
    , REPL ["x"] ExitSuccess (unlines [shortHelp, ": x", "! 'x' is not defined", ": bye"]) ""
    , INI "a = 4; b = 2; f(x, y) = x*10 + y; x = f(a, b);"
    , REPL ["x"] ExitSuccess (unlines [shortHelp, "Loading ucalcTest.ini", ": x", "= 42", ": bye"]) ""
    , INI "a = 4; b = 2; f(x, y) = x*10 + y; x = f(a, b); error here!"
    , REPL ["x"] ExitSuccess (unlines [shortHelp
                                      , "Loading ucalcTest.ini"
                                      , "Error while loading ucalcTest.ini: Error near here!"
                                      , ": x", "! 'x' is not defined", ": bye"]) ""
    , INI ""
    ]
