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

{- The module Expression represents and evaluates expressions.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expression
    ( Expr(..), Conf, State
    , eval
    , emptyState, emptyConf
    , setMTime, dec, hex, oct, bin, float, size, mtime
    ) where

import Data.Ratio
import Data.Bits
import Data.Maybe
import Text.Show.Functions()
import Data.Time.Clock
import qualified Data.Map as Map

import IEEE754
import Help
import ASCII

-- Type of expressions
data Expr = E String            -- errors
          | Z Integer           -- integral numbers
          | Q (Ratio Integer)   -- rational numbers
          | R Double            -- real numbers
          | B Bool              -- booleans
          | S String            -- strings

          -- blocks
          | Seq Expr Expr               -- sequence of two expressions
          | None                         -- empty expression

          -- functions
          | F String [Expr]             -- function call: F name arguments
          | Def String [String] Expr    -- function definition: Def name arguments value

          -- ternary operator
          | Tern Expr Expr Expr         -- Tern condition value_if_true value_if_false

          -- boolean operations
          | Or Expr Expr | Xor Expr Expr | And Expr Expr | Not Expr

          -- comparison operations
          | Le Expr Expr | Lt Expr Expr | Ge Expr Expr | Gt Expr Expr | Eq Expr Expr | Ne Expr Expr

          -- bitwise operations
          | BitOr Expr Expr | BitXor Expr Expr | BitAnd Expr Expr
          | LShift Expr Expr | RShift Expr Expr
          | BitNeg Expr

          -- arithmetic operations
          | Add Expr Expr       -- x + y
          | Sub Expr Expr       -- x - y
          | Mul Expr Expr       -- x * y
          | Div Expr Expr       -- x / y (real division)
          | Quot Expr Expr      -- x // y (euclidian division)
          | Mod Expr Expr       -- x % y (euclidian division, remainder)
          | Pos Expr            -- +x
          | Neg Expr            -- -x
          | Pow Expr Expr       -- x**y (exponentiation)

          -- display mode
          | SetHex (Maybe Int)  -- hex<n>: hexadecimal mode (on n bits)
          | SetDec (Maybe Int)  -- dec<n>: decimal mode (on n bits)
          | SetOct (Maybe Int)  -- oct<n>: octal mode (on n bits)
          | SetBin (Maybe Int)  -- bin<n>: binary mode (on n bits)
          | SetFlt (Maybe Int)  -- float<n>: IEEE 754 floating point mode (on n bits)
          | Reset               -- reset the display mode
          | Previous            -- display the previous value (e.g. when the current expression only changes the dispay mode)

          -- builtin function evaluation patterns
          -- function: R -> R
          | F_RR String (Double -> Double)
          -- function: (R, R) -> R
          | F_RRR String (Double -> Double -> Double)
          -- function: R -> Z
          | F_RZ String (Integer -> Integer) (Ratio Integer -> Integer) (Double -> Integer)
          -- function: R -> Q
          | F_RQ String (Integer -> Ratio Integer) (Ratio Integer -> Ratio Integer) (Double -> Ratio Integer)
          -- function: (R,R) -> Q
          | F_RRQ String (Integer -> Double -> Ratio Integer) (Ratio Integer -> Double -> Ratio Integer) (Double -> Double -> Ratio Integer)
          -- function: a -> a (x and f(x) are of the same type)
          | F_AA String (Integer -> Integer) (Ratio Integer -> Ratio Integer) (Double -> Double)
          -- function: (a, a) -> a
          | F_AAA String (Integer -> Integer -> Integer) (Ratio Integer -> Ratio Integer -> Ratio Integer) (Double -> Double -> Double)
          -- function: Z -> R
          | F_ZR String (Integer -> Double)
          -- function: R -> B
          | F_RB String (Double -> Bool)

          -- display a string (e.g. help or ASCII table)
          | Put String String

          -- exit the calculator interactive mode
          | Bye String

          deriving (Show, Eq)

-- current configuration of the calculator
data Conf = Conf
    { size :: Int               -- integer/float size in bits
    , hex :: Bool               -- hexadecimal display mode
    , dec :: Bool               -- decimal display mode
    , oct :: Bool               -- octal display mode
    , bin :: Bool               -- binary display mode
    , float :: Bool             -- floating point display mode
    , mtime :: Maybe UTCTime    -- date of the previously loaded configuration file
    }
    deriving (Show, Eq)

-- the environment is a map containing the global definitions
-- and the current local definitions
-- The key contains the name and the arity of the function to
-- allow functions with variable number of arguments.
-- Constants are function with zero arguments.
type Env = Map.Map (String,Int) Expr

-- a state is made of a configuration and an environment
type State = (Conf, Env)

-- Expression shall be comparable for test purpose.
-- So functions inside expression have to be comparable as well.
-- They are always considered as different (they are only intermediate
-- expression, only the final expression is checked by the test).
instance Eq (a -> b) where
    _ == _ = False

-- The configuration shall belong to the Show class for test purpose.
instance Show UTCTime where
    show _ = "<mtime>"

-- reset the display mode in the current state
resetConf :: State -> State
resetConf (conf, env) = (emptyConf{mtime=mtime conf}, env)

-- set the integer/float size
setSize :: Int -> State -> State
setSize n (conf, env) = (conf{size=n}, env)

-- set the hexadecimal display mode
setHex :: State -> State
setHex (conf, env) = (conf{hex=True}, env)

-- set the octal display mode
setOct :: State -> State
setOct (conf, env) = (conf{oct=True}, env)

-- set the binary display mode
setBin :: State -> State
setBin (conf, env) = (conf{bin=True}, env)

-- set the decimal display mode
setDec :: State -> State
setDec (conf, env) = (conf{dec=True}, env)

-- set the IEEE 754 floating point display mode
setFlt :: State -> State
setFlt (conf, env) = (conf{float=True}, env)

-- set the last modification time of the configuration file
setMTime :: State -> UTCTime -> State
setMTime (conf, env) t = (conf{mtime=Just t}, env)

-- Evaluate 2 expressions in sequence.
-- The second is evaluated in the state produced by the first.
eval2 :: State -> Expr -> Expr -> (State, Expr, Expr)
eval2 st e1 e2 = (st2, e1', e2')
    where
        (st1, e1') = eval st e1
        (st2, e2') = eval st1 e2

-- Evaluate n expressions.
-- Each expression is evaluated in the state produced by the previous one.
evalAll :: State -> [Expr] -> (State, [Expr])
evalAll st [] = (st, [])
evalAll st (e:es) = (st2, e':es')
    where
        (st1, e') = eval st e
        (st2, es') = evalAll st1 es

-- Simplify a rational number.
-- If the denominator is 1, the value is an integer
sQ :: Ratio Integer -> Expr
sQ q = case (numerator q, denominator q) of
        (n, 1) -> Z n
        _ -> Q q

-- Evaluate an expression in a state.
-- eval returns the new state and the value of the expression
eval :: State -> Expr -> (State, Expr)

-- Atomic values
eval st (E err) = (st, E err)
eval st (Z x) = (st, Z x)
eval st (Q x) = (st, Q x)
eval st (R x) = (st, R x)
eval st (B x) = (st, B x)
eval st (S x) = (st, S x)

-- New definition: the definition is added to the current environment
-- A definition has no value, it returns None.
eval (conf, env) def@(Def name args _) = ((conf, Map.insert (name, length args) def env), None)

-- Function application
eval st@(_conf, env) (F func xs) = case getDef st func arity of
        -- if the function definition with the correct arity exists
        -- it is evaluated by pushing arguments on the environment
        -- and evaluating the definition in this new environment
        Just (Def _name formalArgs y) -> ((conf'', env), y')
            where
                args = Map.fromList $ zip (map (\a -> (a,0)) formalArgs) realArgs
                env' = Map.union args env
                ((conf'', _env''), y') = eval (conf', env') y
        -- if the symbol is just a constant it is evaluated in the same
        -- environment (no arguments)
        Just y -> ((conf'', env), y')
            where
                ((conf'', _env''), y') = eval (conf', env) y
        -- if the symbol is not defined it is evaluated as an error.
        Nothing
            | arity == 0 -> (st, E $ "'" ++ func ++ "'" ++ " is not defined")
            | otherwise  -> (st, E $ "'" ++ func ++ "'/" ++ show arity ++ " is not defined")
    where
        -- arity = number of arguments
        arity = length xs
        -- realArgs is the values of the arguments
        ((conf', _), realArgs) = evalAll st xs

-- The value of a sequence is the value of the second expression
eval st (Seq a b) = (st', b') where (st', _, b') = eval2 st a b

-- ...
eval st None = (st, None)

-- the value of cond?x:y is either x or y, according to cond
eval st (Tern cond x y) = case eval st cond of
                            (_, E err) -> (st, E err)
                            (st', B True) -> eval st' x
                            (st', B False) -> eval st' y
                            (_, _) -> (st, E "non boolean condition")

-- boolean operations
eval st (Or x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', B x', B y') -> (st', B (x'||y'))
    (_, _, _) -> (st, E "bad operands for 'or'")
eval st (Xor x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', B x', B y') -> (st', B (x'/=y'))
    (_, _, _) -> (st, E "bad operands for 'xor'")
eval st (And x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', B x', B y') -> (st', B (x'&&y'))
    (_, _, _) -> (st, E "bad operands for 'and'")
eval st (Not x) = case eval st x of
    (_, E err) -> (st, E err)
    (st', B x') -> (st', B (not x'))
    (_, _) -> (st, E "bad operand for 'not'")

-- comparison operations
eval st (Le x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', B (x'<=y'))
    (st', Z x', Q y') -> (st', B (fromInteger x'<=y'))
    (st', Z x', R y') -> (st', B (fromInteger x'<=y'))
    (st', Q x', Z y') -> (st', B (x'<=fromInteger y'))
    (st', Q x', Q y') -> (st', B (x'<=y'))
    (st', Q x', R y') -> (st', B (fromRational x'<=y'))
    (st', R x', Z y') -> (st', B (x'<=fromInteger y'))
    (st', R x', Q y') -> (st', B (x'<=fromRational y'))
    (st', R x', R y') -> (st', B (x'<=y'))
    (st', S x', S y') -> (st', B (x'<=y'))
    (_, _, _) -> (st, E "bad operands for '<='")
eval st (Lt x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', B (x'<y'))
    (st', Z x', Q y') -> (st', B (fromInteger x'<y'))
    (st', Z x', R y') -> (st', B (fromInteger x'<y'))
    (st', Q x', Z y') -> (st', B (x'<fromInteger y'))
    (st', Q x', Q y') -> (st', B (x'<y'))
    (st', Q x', R y') -> (st', B (fromRational x'<y'))
    (st', R x', Z y') -> (st', B (x'<fromInteger y'))
    (st', R x', Q y') -> (st', B (x'<fromRational y'))
    (st', R x', R y') -> (st', B (x'<y'))
    (st', S x', S y') -> (st', B (x'<y'))
    (_, _, _) -> (st, E "bad operands for '<'")
eval st (Ge x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', B (x'>=y'))
    (st', Z x', Q y') -> (st', B (fromInteger x'>=y'))
    (st', Z x', R y') -> (st', B (fromInteger x'>=y'))
    (st', Q x', Z y') -> (st', B (x'>=fromInteger y'))
    (st', Q x', Q y') -> (st', B (x'>=y'))
    (st', Q x', R y') -> (st', B (fromRational x'>=y'))
    (st', R x', Z y') -> (st', B (x'>=fromInteger y'))
    (st', R x', Q y') -> (st', B (x'>=fromRational y'))
    (st', R x', R y') -> (st', B (x'>=y'))
    (st', S x', S y') -> (st', B (x'>=y'))
    (_, _, _) -> (st, E "bad operands for '>='")
eval st (Gt x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', B (x'>y'))
    (st', Z x', Q y') -> (st', B (fromInteger x'>y'))
    (st', Z x', R y') -> (st', B (fromInteger x'>y'))
    (st', Q x', Z y') -> (st', B (x'>fromInteger y'))
    (st', Q x', Q y') -> (st', B (x'>y'))
    (st', Q x', R y') -> (st', B (fromRational x'>y'))
    (st', R x', Z y') -> (st', B (x'>fromInteger y'))
    (st', R x', Q y') -> (st', B (x'>fromRational y'))
    (st', R x', R y') -> (st', B (x'>y'))
    (st', S x', S y') -> (st', B (x'>y'))
    (_, _, _) -> (st, E "bad operands for '>'")
eval st (Eq x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', B (x'==y'))
    (st', Z x', Q y') -> (st', B (fromInteger x'==y'))
    (st', Z x', R y') -> (st', B (fromInteger x'==y'))
    (st', Q x', Z y') -> (st', B (x'==fromInteger y'))
    (st', Q x', Q y') -> (st', B (x'==y'))
    (st', Q x', R y') -> (st', B (fromRational x'==y'))
    (st', R x', Z y') -> (st', B (x'==fromInteger y'))
    (st', R x', Q y') -> (st', B (x'==fromRational y'))
    (st', R x', R y') -> (st', B (x'==y'))
    (st', S x', S y') -> (st', B (x'==y'))
    (_, _, _) -> (st, E "bad operands for '=='")
eval st (Ne x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', B (x'/=y'))
    (st', Z x', Q y') -> (st', B (fromInteger x'/=y'))
    (st', Z x', R y') -> (st', B (fromInteger x'/=y'))
    (st', Q x', Z y') -> (st', B (x'/=fromInteger y'))
    (st', Q x', Q y') -> (st', B (x'/=y'))
    (st', Q x', R y') -> (st', B (fromRational x'/=y'))
    (st', R x', Z y') -> (st', B (x'/=fromInteger y'))
    (st', R x', Q y') -> (st', B (x'/=fromRational y'))
    (st', R x', R y') -> (st', B (x'/=y'))
    (st', S x', S y') -> (st', B (x'/=y'))
    (_, _, _) -> (st, E "bad operands for '!='")

-- bitwise operations
eval st (BitOr x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (setHex st', Z (x'.|.y'))
    (_, _, _) -> (st, E "bad operands for '|'")
eval st (BitXor x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (setHex st', Z (x'`xor`y'))
    (_, _, _) -> (st, E "bad operands for '^'")
eval st (BitAnd x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (setHex st', Z (x'.&.y'))
    (_, _, _) -> (st, E "bad operands for '&'")
eval st (LShift x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (setHex st', Z (x' `shift` fromInteger y'))
    (_, _, _) -> (st, E "bad operands for '<<'")
eval st (RShift x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (setHex st', Z (x' `shift` negate (fromInteger y')))
    (_, _, _) -> (st, E "bad operands for '>>'")
eval st (BitNeg x) = case eval st x of
    (_, E err) -> (st, E err)
    (st', Z x') -> (setHex st', Z (complement x'))
    (_, _) -> (st, E "bad operand for '~'")

-- arithmetic operations
eval st (Add x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', Z (x'+y'))
    (st', Z x', Q y') -> (st', sQ (fromInteger x'+y'))
    (st', Z x', R y') -> (st', R (fromInteger x'+y'))
    (st', Q x', Z y') -> (st', sQ (x'+fromInteger y'))
    (st', Q x', Q y') -> (st', sQ (x'+y'))
    (st', Q x', R y') -> (st', R (fromRational x'+y'))
    (st', R x', Z y') -> (st', R (x'+fromInteger y'))
    (st', R x', Q y') -> (st', R (x'+fromRational y'))
    (st', R x', R y') -> (st', R (x'+y'))
    (st', S x', y') -> (st', S (x' ++ pp' y'))
    (st', x', S y') -> (st', S (pp' x' ++ y'))
    (_, _, _) -> (st, E "bad operands for '+'")
eval st (Sub x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', Z (x'-y'))
    (st', Z x', Q y') -> (st', sQ (fromInteger x'-y'))
    (st', Z x', R y') -> (st', R (fromInteger x'-y'))
    (st', Q x', Z y') -> (st', sQ (x'-fromInteger y'))
    (st', Q x', Q y') -> (st', sQ (x'-y'))
    (st', Q x', R y') -> (st', R (fromRational x'-y'))
    (st', R x', Z y') -> (st', R (x'-fromInteger y'))
    (st', R x', Q y') -> (st', R (x'-fromRational y'))
    (st', R x', R y') -> (st', R (x'-y'))
    (_, _, _) -> (st, E "bad operands for '-'")
eval st (Mul x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', Z (x'*y'))
    (st', Z x', Q y') -> (st', sQ (fromInteger x'*y'))
    (st', Z x', R y') -> (st', R (fromInteger x'*y'))
    (st', Q x', Z y') -> (st', sQ (x'*fromInteger y'))
    (st', Q x', Q y') -> (st', sQ (x'*y'))
    (st', Q x', R y') -> (st', R (fromRational x'*y'))
    (st', R x', Z y') -> (st', R (x'*fromInteger y'))
    (st', R x', Q y') -> (st', R (x'*fromRational y'))
    (st', R x', R y') -> (st', R (x'*y'))
    (st', S x', Z y') -> (st', S (concat $ replicate (fromInteger y') x'))
    (st', Z x', S y') -> (st', S (concat $ replicate (fromInteger x') y'))
    (_, _, _) -> (st, E "bad operands for '*'")
eval st (Div x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z _, Z 0)   -> (st', E "Zero division")
    (st', Q _, Z 0)   -> (st', E "Zero division")
    (st', Z _, Q 0)   -> (st', E "Zero division")
    (st', Q _, Q 0)   -> (st', E "Zero division")
    (st', Z x', Z y') -> (st', sQ (x'%y'))
    (st', Z x', Q y') -> (st', sQ (fromInteger x'/y'))
    (st', Z x', R y') -> (st', R (fromInteger x'/y'))
    (st', Q x', Z y') -> (st', sQ (x'/fromInteger y'))
    (st', Q x', Q y') -> (st', sQ (x'/y'))
    (st', Q x', R y') -> (st', R (fromRational x'/y'))
    (st', R x', Z y') -> (st', R (x'/fromInteger y'))
    (st', R x', Q y') -> (st', R (x'/fromRational y'))
    (st', R x', R y') -> (st', R (x'/y'))
    (_, _, _) -> (st, E "bad operands for '/'")
eval st (Quot x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (_, _, Z 0) -> (st, E "Zero division")
    (st', Z x', Z y') -> (st', Z (x' `div` y'))
    (_, _, _) -> (st, E "bad operands for '//'")
eval st (Mod x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (_, _, Z 0) -> (st, E "Zero division")
    (st', Z x', Z y') -> (st', Z (x' `mod` y'))
    (_, _, _) -> (st, E "bad operands for '%'")

eval st (Pos x) = case eval st x of
    (_, E err) -> (st, E err)
    (st', Z x') -> (st', Z x')
    (st', Q x') -> (st', Q x')
    (st', R x') -> (st', R x')
    (_, _) -> (st, E "bad operand for '+'")
eval st (Neg x) = case eval st x of
    (_, E err) -> (st, E err)
    (st', Z x') -> (st', Z (negate x'))
    (st', Q x') -> (st', Q (negate x'))
    (st', R x') -> (st', R (negate x'))
    (_, _) -> (st, E "bad operand for '-'")

eval st (Pow x y) = case eval2 st x y of
    (_, E err, _) -> (st, E err)
    (_, _, E err) -> (st, E err)
    (st', Z x', Z y') -> (st', Z (x'^y'))
    (st', Z x', Q y') -> (st', R (fromInteger x'**fromRational y'))
    (st', Z x', R y') -> (st', R (fromInteger x'**y'))
    (st', Q x', Z y') -> (st', sQ ((n^y') % (d^y'))) where { n = numerator x'; d = denominator x' }
    (st', Q x', Q y') -> (st', R (fromRational x'**fromRational y'))
    (st', Q x', R y') -> (st', R (fromRational x'**y'))
    (st', R x', Z y') -> (st', R (x'**fromInteger y'))
    (st', R x', Q y') -> (st', R (x'**fromRational y'))
    (st', R x', R y') -> (st', R (x'**y'))
    (_, _, _) -> (st, E "bad operands for '**'")

-- display mode
eval st Reset = (resetConf st, Previous)
eval st (SetDec (Just n)) = ((setSize n . setDec) st, Previous)
eval st (SetHex (Just n)) = ((setSize n . setHex) st, Previous)
eval st (SetOct (Just n)) = ((setSize n . setOct) st, Previous)
eval st (SetBin (Just n)) = ((setSize n . setBin) st, Previous)
eval st (SetFlt (Just n)) = ((setSize n . setFlt) st, Previous)
eval st (SetDec Nothing) = (setDec st, Previous)
eval st (SetHex Nothing) = (setHex st, Previous)
eval st (SetOct Nothing) = (setOct st, Previous)
eval st (SetBin Nothing) = (setBin st, Previous)
eval st (SetFlt Nothing) = (setFlt st, Previous)

eval st Previous = (st, Previous)

-- builtin function evaluation patterns

-- functions :: R -> R
eval st (F_RR name f) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', R $ f $ fromInteger x)
    (st', [Q x]) -> (st', R $ f $ fromRational x)
    (st', [R x]) -> (st', R $ f x)
    (_, _) -> (st, E $ "bad operand for "++name)

-- functions :: R -> Z
eval st (F_RZ name fZ fQ fR) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', Z $ fZ x)
    (st', [Q x]) -> (st', Z $ fQ x)
    (st', [R x]) -> (st', Z $ fR x)
    (_, _) -> (st, E $ "bad operand for "++name)

-- functions :: R -> Q
eval st (F_RQ name fZ fQ fR) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', sQ $ fZ x)
    (st', [Q x]) -> (st', sQ $ fQ x)
    (st', [R x]) -> (st', sQ $ fR x)
    (_, _) -> (st, E $ "bad operand for "++name)

-- functions :: (R,R) -> Q
eval st (F_RRQ name fZ fQ fR) = case evalBuiltinArgs 2 st of
    (_, [E err, _]) -> (st, E err)
    (_, [_, E err]) -> (st, E err)
    (st', [Z x, Z y]) -> (st', sQ $ fZ x (fromInteger y))
    (st', [Z x, Q y]) -> (st', sQ $ fZ x (fromRational y))
    (st', [Z x, R y]) -> (st', sQ $ fZ x y)
    (st', [Q x, Z y]) -> (st', sQ $ fQ x (fromInteger y))
    (st', [Q x, Q y]) -> (st', sQ $ fQ x (fromRational y))
    (st', [Q x, R y]) -> (st', sQ $ fQ x y)
    (st', [R x, Z y]) -> (st', sQ $ fR x (fromInteger y))
    (st', [R x, Q y]) -> (st', sQ $ fR x (fromRational y))
    (st', [R x, R y]) -> (st', sQ $ fR x y)
    (_, _) -> (st, E $ "bad operands for "++name)

-- functions :: a -> a
eval st (F_AA name fZ fQ fR) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', Z $ fZ x)
    (st', [Q x]) -> (st', Q $ fQ x)
    (st', [R x]) -> (st', R $ fR x)
    (_, _) -> (st, E $ "bad operand for "++name)

-- functions :: (a,a) -> a
eval st (F_AAA name fZ fQ fR) = case evalBuiltinArgs 2 st of
    (_, [E err, _]) -> (st, E err)
    (_, [_, E err]) -> (st, E err)
    (st', [Z x, Z y]) -> (st', Z $ fZ x y)
    (st', [Z x, Q y]) -> (st', sQ $ fQ (fromInteger x) y)
    (st', [Z x, R y]) -> (st', R $ fR (fromInteger x) y)
    (st', [Q x, Z y]) -> (st', sQ $ fQ x (fromInteger y))
    (st', [Q x, Q y]) -> (st', sQ $ fQ x y)
    (st', [Q x, R y]) -> (st', R $ fR (fromRational x) y)
    (st', [R x, Z y]) -> (st', R $ fR x (fromInteger y))
    (st', [R x, Q y]) -> (st', R $ fR x (fromRational y))
    (st', [R x, R y]) -> (st', R $ fR x y)
    (_, _) -> (st, E $ "bad operands for "++name)

-- functions :: (R,R) -> R
eval st (F_RRR name f) = case evalBuiltinArgs 2 st of
    (_, [E err, _]) -> (st, E err)
    (_, [_, E err]) -> (st, E err)
    (st', [Z x, Z y]) -> (st', R $ f (fromInteger x) (fromInteger y))
    (st', [Z x, Q y]) -> (st', R $ f (fromInteger x) (fromRational y))
    (st', [Z x, R y]) -> (st', R $ f (fromInteger x) y)
    (st', [Q x, Z y]) -> (st', R $ f (fromRational x) (fromInteger y))
    (st', [Q x, Q y]) -> (st', R $ f (fromRational x) (fromRational y))
    (st', [Q x, R y]) -> (st', R $ f (fromRational x) y)
    (st', [R x, Z y]) -> (st', R $ f x (fromInteger y))
    (st', [R x, Q y]) -> (st', R $ f x (fromRational y))
    (st', [R x, R y]) -> (st', R $ f x y)
    (_, _) -> (st, E $ "bad operands for "++name)

-- functions :: Z -> R
eval st (F_ZR name f) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', R $ f x)
    (_, _) -> (st, E $ "bad operand for "++name)

-- functions :: R -> B
eval st (F_RB name f) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', B $ f $ fromInteger x)
    (st', [Q x]) -> (st', B $ f $ fromRational x)
    (st', [R x]) -> (st', B $ f x)
    (_, _) -> (st, E $ "bad operand for "++name)

-- commands for the interface
-- display a string
eval st (Put name f) = (st, Put name f)
-- exit the calculator
eval st (Bye name) = (st, Bye name)

-- evaluate the arguments of a builtin function
evalBuiltinArgs :: Int -> State -> (State, [Expr])
evalBuiltinArgs n st@(_, env) = evalAll st exprs
    where
        -- args = names of the formal arguments
        args = builtinArgs n
        -- exprs = expressions associated to the formal arguments in the current environment
        exprs = map (\arg -> fromMaybe (E "unexpected error: invalid formal argument") $ Map.lookup (arg,0) env) args

-- default (minimal) configuration
emptyConf :: Conf
emptyConf = Conf { size = 0
                 , hex = False, dec = False, oct = False, bin = False
                 , float = False
                 , mtime = Nothing
                 }

-- default state = default configuration + empty environment
emptyState :: State
emptyState = (emptyConf, Map.empty)

-- accepted bit sizes for integral values
bitSizes :: [Int]
bitSizes = [8, 16, 32, 64]

-- accepted bit sizes for floating point values
floatSizes :: [Int]
floatSizes = [32, 64]

-- builtin constants and functions
builtin :: Env
builtin = Map.fromList $
              [ constant "true"     $ B True
              , constant "false"    $ B False
              , constant "pi"       $ R pi
              , constant "e"        $ R $ exp 1
              , constant "nan"      $ R $ 0.0/0.0
              , constant "inf"      $ R $ 1.0/0.0

              , funcRZ "int" id floor floor
              , funcRR "float" id
              , funcRQ "rat" fromInteger id toRational
              , funcRRQ "rat" (\x _ -> fromInteger x) const approxRational

              , funcAA "abs" abs abs abs
              , funcRZ "ceil" id ceiling ceiling
              , funcRZ "floor" id floor floor
              , funcRZ "round" id round round
              , funcRZ "trunc" id truncate truncate
              , funcRR "mantissa" mantissa
              , funcRZ "exponent" (fromIntegral.exponent.(fromInteger::Integer->Double))
                                  (fromIntegral.exponent.(fromRational::Ratio Integer->Double))
                                  (fromIntegral.exponent)
              , funcRR "fract" (snd.(properFraction::Double->(Integer,Double)))
              , funcAAA "min" min min min
              , funcAAA "max" max max max

              , funcAA "sqr" (^(2::Integer)) (^(2::Integer)) (**2)
              , funcRR "sqrt" sqrt
              , funcRR "cbrt" (**(1/3))

              , funcRR "cos" cos
              , funcRR "acos" acos
              , funcRR "cosh" cosh
              , funcRR "sin" sin
              , funcRR "asin" asin
              , funcRR "sinh" sinh
              , funcRR "tan" tan
              , funcRR "atan" atan
              , funcRR "tanh" tanh
              , funcRRR "atan" atan2
              , funcRRR "atan2" atan2
              , funcRR "deg" (*(180/pi))
              , funcRR "rad" (*(pi/180))

              , funcRR "exp" exp
              , funcRR "log" log
              , funcRR "ln" log
              , funcRR "log10" (logBase 10)
              , funcRR "log2" (logBase 2)
              , funcRRR "log" logBase

              , funcRZ "float2ieee" (floatToInteger.fromInteger) (floatToInteger.fromRational) (floatToInteger.doubleToFloat)
              , funcZR "ieee2float" (floatToDouble.integerToFloat)
              , funcRZ "double2ieee" (doubleToInteger.fromInteger) (doubleToInteger.fromRational) doubleToInteger
              , funcZR "ieee2double" integerToDouble

              , funcRB "isfinite" (\x -> not (isNaN x) && not (isInfinite x))
              , funcRB "isinf" isInfinite
              , funcRB "isnan" isNaN

              , funcBye "bye"
              , funcBye "exit"
              , funcPut "help" $ unlines ["", shortHelp, "", longHelp]
              , funcPut "license" $ unlines ["", license]
              , funcPut "ascii" $ unlines ["", ascii]
              ]
              ++ [ constant "reset"             Reset ]
              ++ [ constant "hex"               $ SetHex Nothing ]
              ++ [ constant ("hex"++show b)     $ SetHex $ Just b | b <- bitSizes ]
              ++ [ constant "dec"               $ SetDec Nothing ]
              ++ [ constant ("dec"++show b)     $ SetDec $ Just b | b <- bitSizes ]
              ++ [ constant "oct"               $ SetOct Nothing ]
              ++ [ constant ("oct"++show b)     $ SetOct $ Just b | b <- bitSizes ]
              ++ [ constant "bin"               $ SetBin Nothing ]
              ++ [ constant ("bin"++show b)     $ SetBin $ Just b | b <- bitSizes ]
              ++ [ constant "float"             $ SetFlt Nothing ]
              ++ [ constant ("float"++show b)   $ SetFlt $ Just b | b <- floatSizes ]

-- char used to store formal argument names of builtin functions in the environment.
-- This character is not a valid char for identifiers so as not to interfere
-- with existing names.
builtinArgTag :: Char
builtinArgTag = '@'

-- make a builtin function definition for the builtin environment.
builtinFunction :: String -> Int -> Expr -> ((String, Int), Expr)
builtinFunction name arity f = ((name, arity), Def name (builtinArgs arity) f)

-- list of formal argument names for builtin functions
builtinArgs :: Int -> [String]
builtinArgs n = [ builtinArgTag:show i | i <- [1..n]]

-- make a constant
constant :: String -> Expr -> ((String, Int), Expr)
constant name value = ((name, 0), value)

-- make a builtin function of type R -> R
funcRR :: String
       -> (Double -> Double)
       -> ((String, Int), Expr)
funcRR name f = builtinFunction name 1 $ F_RR name f

-- make a builtin function of type R -> Z
funcRZ :: String
       -> (Integer -> Integer)
       -> (Ratio Integer -> Integer)
       -> (Double -> Integer)
       -> ((String, Int), Expr)
funcRZ name fZ fQ fR = builtinFunction name 1 $ F_RZ name fZ fQ fR

-- make a builtin function of type R -> Q
funcRQ :: String
       -> (Integer -> Ratio Integer)
       -> (Ratio Integer -> Ratio Integer)
       -> (Double -> Ratio Integer)
       -> ((String, Int), Expr)
funcRQ name fZ fQ fR = builtinFunction name 1 $ F_RQ name fZ fQ fR

-- make a builtin function of type (R,R) -> Q
funcRRQ :: String
        -> (Integer -> Double -> Ratio Integer)
        -> (Ratio Integer -> Double -> Ratio Integer)
        -> (Double -> Double -> Ratio Integer)
        -> ((String, Int), Expr)
funcRRQ name fZ fQ fR = builtinFunction name 2 $ F_RRQ name fZ fQ fR

-- make a builtin function of type a -> a
funcAA :: String
       -> (Integer -> Integer)
       -> (Ratio Integer -> Ratio Integer)
       -> (Double -> Double)
       -> ((String, Int), Expr)
funcAA name fZ fQ fR = builtinFunction name 1 $ F_AA name fZ fQ fR

-- make a builtin function of type (a,a) -> a
funcAAA :: String
        -> (Integer -> Integer -> Integer)
        -> (Ratio Integer -> Ratio Integer -> Ratio Integer)
        -> (Double -> Double -> Double)
        -> ((String, Int), Expr)
funcAAA name fZ fQ fR = builtinFunction name 2 $ F_AAA name fZ fQ fR

-- make a builtin function of type (R,R) -> R
funcRRR :: String -> (Double -> Double -> Double) -> ((String, Int), Expr)
funcRRR name f = builtinFunction name 2 $ F_RRR name f

-- make a builtin function of type Z -> R
funcZR :: String -> (Integer -> Double) -> ((String, Int), Expr)
funcZR name f = builtinFunction name 1 $ F_ZR name f

-- make a builtin function of type R -> B
funcRB :: String -> (Double -> Bool) -> ((String, Int), Expr)
funcRB name f = builtinFunction name 1 $ F_RB name f

-- make a builtin function that prints a string
funcPut :: String -> String -> ((String, Int), Expr)
funcPut name f = builtinFunction name 0 $ Put name f

-- make a builtin function that ends the calculator
funcBye :: String -> ((String, Int), Expr)
funcBye name = builtinFunction name 0 $ Bye name

-- mantissa of a floating point value
mantissa :: Double -> Double
mantissa x = x / (2.0 ** fromIntegral (exponent x))

-- look for a definition in the current environment with a specific arity
-- The definition is searched in the current environment
-- and then in the builtin environment
getDef :: State -> String -> Int -> Maybe Expr
getDef (_, env) name arity = case Map.lookup (name, arity) env of
                                Nothing -> Map.lookup (name, arity) builtin
                                x -> x

-- pp' converts an already evaluated expression to a string
pp' :: Expr -> String
pp' (Z x) = show x

pp' (Q x) = show n ++ "/" ++ show d
    where
        n = numerator x
        d = denominator x

pp' (R x) = show x

pp' (B True) = "true"
pp' (B False) = "false"

pp' (S s) = s

pp' None = ""

pp' x = "<"++show x++">"