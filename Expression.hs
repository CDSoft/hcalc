{-# OPTIONS_GHC -fno-warn-orphans #-}

module Expression where

import Data.Ratio
import Data.Bits
-- import Data.List
-- import Data.Char
import Data.Maybe
import Text.Show.Functions()

import IEEE754
import Help
import ASCII

data Expr = E String
          | Z Integer
          | Q (Ratio Integer)
          | R Double
          | B Bool
          | S String
          | F String [Expr]
          | Seq Expr Expr | Nop
          | Def String [String] Expr
          | Tern Expr Expr Expr
          | Or Expr Expr | Xor Expr Expr | And Expr Expr | Not Expr
          | Le Expr Expr | Lt Expr Expr | Ge Expr Expr | Gt Expr Expr | Eq Expr Expr | Ne Expr Expr
          | BitOr Expr Expr | BitXor Expr Expr | BitAnd Expr Expr
          | LShift Expr Expr | RShift Expr Expr
          | Add Expr Expr | Sub Expr Expr
          | Mul Expr Expr | Div Expr Expr | Quot Expr Expr | Mod Expr Expr
          | Pos Expr | Neg Expr | BitNeg Expr
          | Pow Expr Expr
          | SetHex (Maybe Int) | SetDec (Maybe Int) | SetOct (Maybe Int) | SetBin (Maybe Int) | SetFlt (Maybe Int) | Reset
          | Previous
          | F_RR String (Double -> Double)
          | F_RRR String (Double -> Double -> Double)
          | F_RZ String (Integer -> Integer) (Ratio Integer -> Integer) (Double -> Integer)
          | F_RQ String (Integer -> Ratio Integer) (Ratio Integer -> Ratio Integer) (Double -> Ratio Integer)
          | F_AA String (Integer -> Integer) (Ratio Integer -> Ratio Integer) (Double -> Double)
          | F_AAA String (Integer -> Integer -> Integer) (Ratio Integer -> Ratio Integer -> Ratio Integer) (Double -> Double -> Double)
          | F_ZR String (Integer -> Double)
          | F_RB String (Double -> Bool)
          | Put String String
          | Bye String
          deriving (Show, Eq)

data Conf = Conf
    { size :: Int
    , hex :: Bool
    , dec :: Bool
    , oct :: Bool
    , bin :: Bool
    , float :: Bool
    }
    deriving (Show, Eq)

type Env = [(String, Expr)]
type State = (Conf, Env, Env) -- (conf, global, local)

instance Eq (a -> b) where
    _ == _ = False

-- need functions to evaluae sequences and propagate states and errors

resetConf :: State -> State
resetConf (_, global, local) = (emptyConf, global, local)

setSize :: Int -> State -> State
setSize n (conf, global, local) = (conf{size=n}, global, local)

setHex :: State -> State
setHex (conf, global, local) = (conf{hex=True}, global, local)

setOct :: State -> State
setOct (conf, global, local) = (conf{oct=True}, global, local)

setBin :: State -> State
setBin (conf, global, local) = (conf{bin=True}, global, local)

setDec :: State -> State
setDec (conf, global, local) = (conf{dec=True}, global, local)

setFlt :: State -> State
setFlt (conf, global, local) = (conf{float=True}, global, local)

setLocal :: Env -> (State, Expr) -> (State, Expr)
setLocal local ((conf, global, _), expr) = ((conf, global, local), expr)

eval2 :: State -> Expr -> Expr -> (State, Expr, Expr)
eval2 st e1 e2 = (st2, e1', e2')
    where
        (st1, e1') = eval st e1
        (st2, e2') = eval st1 e2

evalAll :: State -> [Expr] -> (State, [Expr])
evalAll st [] = (st, [])
evalAll st (e:es) = (st2, e':es')
    where
        (st1, e') = eval st e
        (st2, es') = evalAll st1 es

sQ :: Ratio Integer -> Expr
sQ q = case (numerator q, denominator q) of
        (n, 1) -> Z n
        _ -> Q q

eval :: State -> Expr -> (State, Expr)

eval st (E err) = (st, E err)
eval st (Z x) = (st, Z x)
eval st (Q x) = (st, Q x)
eval st (R x) = (st, R x)
eval st (B x) = (st, B x)
eval st (S x) = (st, S x)

eval st def@(Def name _ _) = (setState st name def, Nop)

-- un appel, même sans paramètres doit empiler quelque chose dans local (ex : un flag quelconque ? une string avec le nom de la fonction courante)
-- TODO : un appel doit ajouter la fonction courante dans l'env local pour permettre les fonctions récursives
eval st@(_conf, _global, local) (F func xs) = case getState st func arity  of
        Just def@(Def name formalArgs y) -> setLocal local $ eval st'' y -- undefined -- ajouter les parameters dans l'état local systématiquement ! (ie pas avec setState)
                                                where
                                                    st'' = (conf', global', args ++ [(name, def), (frameTag, S func)] {- ++ local -})
                                                    args = zip formalArgs realArgs
        Just y -> setLocal local $ eval st'' y
                                                where
                                                    st'' = (conf', global', [(frameTag, S func)] {- ++ local -})
        Nothing -> (st, E $ "'" ++ func ++ "'/" ++ show arity ++ " is not defined")
    where
        arity = length xs
        ((conf', global', _), realArgs) = evalArgs st xs
        frameTag = "$frame"
        evalArgs st0 [] = (st0, [])
        evalArgs st0 (arg:args) = (stxs, arg':args')
            where
                (stx, arg') = eval st0 arg
                (stxs, args') = evalArgs stx args

eval st (Seq a b) = (stb, b') where { (sta, _) = eval st a; (stb, b') = eval sta b }
eval st Nop = (st, Nop)

eval st (Tern cond x y) = case eval st cond of
                            (_, E err) -> (st, E err)
                            (st', B True) -> eval st' x
                            (st', B False) -> eval st' y
                            (_, _) -> (st, E "non boolean condition")

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

eval st (BitNeg x) = case eval st x of
    (_, E err) -> (st, E err)
    (st', Z x') -> (setHex st', Z (complement x'))
    (_, _) -> (st, E "bad operand for '~'")

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

eval st (F_RR name f) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', R $ f $ fromInteger x)
    (st', [Q x]) -> (st', R $ f $ fromRational x)
    (st', [R x]) -> (st', R $ f x)
    (_, _) -> (st, E $ "bad operand for "++name)

eval st (F_RZ name fZ fQ fR) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', Z $ fZ x)
    (st', [Q x]) -> (st', Z $ fQ x)
    (st', [R x]) -> (st', Z $ fR x)
    (_, _) -> (st, E $ "bad operand for "++name)

eval st (F_RQ name fZ fQ fR) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', sQ $ fZ x)
    (st', [Q x]) -> (st', sQ $ fQ x)
    (st', [R x]) -> (st', sQ $ fR x)
    (_, _) -> (st, E $ "bad operand for "++name)

eval st (F_AA name fZ fQ fR) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', Z $ fZ x)
    (st', [Q x]) -> (st', Q $ fQ x)
    (st', [R x]) -> (st', R $ fR x)
    (_, _) -> (st, E $ "bad operand for "++name)

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
    (_, _) -> (st, E $ "bad operand for "++name)

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
    (_, _) -> (st, E $ "bad operand for "++name)

eval st (F_ZR name f) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', R $ f x)
    (_, _) -> (st, E $ "bad operand for "++name)

eval st (F_RB name f) = case evalBuiltinArgs 1 st of
    (_, [E err]) -> (st, E err)
    (st', [Z x]) -> (st', B $ f $ fromInteger x)
    (st', [Q x]) -> (st', B $ f $ fromRational x)
    (st', [R x]) -> (st', B $ f x)
    (_, _) -> (st, E $ "bad operand for "++name)

eval st (Put name f) = (st, Put name f)
eval st (Bye name) = (st, Bye name)

--eval st (F_Int name) = case evalBuiltinArgs 1 st of
--    (_, [E err]) -> (st, E err)
--    (st', [Z x]) -> (st', Z $ x)
--    (st', [Q x]) -> (st', Z $ floor $ fromRational x)
--    (st', [R x]) -> (st', Z $ floor x)
--    (_, _) -> (st, E $ "bad operand for "++name)

--eval st (F_Float name) = case evalBuiltinArgs 1 st of
--    (_, [E err]) -> (st, E err)
--    (st', [Z x]) -> (st', R $ fromInteger x)
--    (st', [Q x]) -> (st', R $ fromRational x)
--    (st', [R x]) -> (st', R $ x)
--    (_, _) -> (st, E $ "bad operand for "++name)

--eval st (F_Rat name) = case evalBuiltinArgs 1 st of
--    (_, [E err]) -> (st, E err)
--    (st', [Z x]) -> (st', Q $ fromInteger x)
--    (st', [Q x]) -> (st', Q x)
--    (st', [R x]) -> (st', Q $ toRational x)
--    (_, _) -> (st, E $ "bad operand for "++name)

evalBuiltinArgs :: Int -> State -> (State, [Expr])
evalBuiltinArgs n st@(_, _, local) = evalAll st exprs
    where
        args = [ builtinArgTag:show i | i <- [1..n] ]
        exprs = map (\arg -> fromMaybe undefined $ lookup arg local) args

-- The evaluation can not go further, expr is either an atom, either unevaluable (???)
--eval st expr = (st, expr) -- => non, sinon ghc ne sgnalera pas les cas non implémentés

emptyConf :: Conf
emptyConf = Conf {size = 0, hex = False, dec = False, oct = False, bin = False, float = False}

emptyState :: State
emptyState = (emptyConf, [], [])

bitSizes :: [Int]
bitSizes = [8, 16, 32, 64]

floatSizes :: [Int]
floatSizes = [32, 64]

builtin :: Env
builtin = [ ("true", B True)
          , ("false", B False)
          , ("pi", R pi)
          , ("e", R $ exp 1)
          , ("nan", R $ 0.0/0.0)
          , ("inf", R $ 1.0/0.0)

          , funcRZ "int" id floor floor
          , funcRR "float" id
          , funcRQ "rat" fromInteger id toRational

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
          , funcPut "help" help
          , funcPut "ascii" ascii
          ]
          ++ [ ("reset", Reset) ]
          ++ [ ("hex", SetHex Nothing) ]
          ++ [ ("hex"++show b, SetHex $ Just b) | b <- bitSizes ]
          ++ [ ("dec", SetDec Nothing) ]
          ++ [ ("dec"++show b, SetDec $ Just b) | b <- bitSizes ]
          ++ [ ("oct", SetOct Nothing) ]
          ++ [ ("oct"++show b, SetOct $ Just b) | b <- bitSizes ]
          ++ [ ("bin", SetBin Nothing) ]
          ++ [ ("bin"++show b, SetBin $ Just b) | b <- bitSizes ]
          ++ [ ("float", SetFlt Nothing) ]
          ++ [ ("float"++show b, SetFlt $ Just b) | b <- floatSizes ]

builtinArgTag :: Char
builtinArgTag = '@'

builtinFunction :: String -> Int -> Expr -> (String, Expr)
builtinFunction name arity f = (name, Def name args f)
    where
        args = [ builtinArgTag:show i | i <- [1..arity] ]

funcRR :: String
       -> (Double -> Double)
       -> (String, Expr)
funcRR name f = builtinFunction name 1 $ F_RR name f

funcRZ :: String
       -> (Integer -> Integer)
       -> (Ratio Integer -> Integer)
       -> (Double -> Integer)
       -> (String, Expr)
funcRZ name fZ fQ fR = builtinFunction name 1 $ F_RZ name fZ fQ fR

funcRQ :: String
       -> (Integer -> Ratio Integer)
       -> (Ratio Integer -> Ratio Integer)
       -> (Double -> Ratio Integer)
       -> (String, Expr)
funcRQ name fZ fQ fR = builtinFunction name 1 $ F_RQ name fZ fQ fR

funcAA :: String
       -> (Integer -> Integer)
       -> (Ratio Integer -> Ratio Integer)
       -> (Double -> Double)
       -> (String, Expr)
funcAA name fZ fQ fR = builtinFunction name 1 $ F_AA name fZ fQ fR

funcAAA :: String
        -> (Integer -> Integer -> Integer)
        -> (Ratio Integer -> Ratio Integer -> Ratio Integer)
        -> (Double -> Double -> Double)
        -> (String, Expr)
funcAAA name fZ fQ fR = builtinFunction name 2 $ F_AAA name fZ fQ fR

funcRRR :: String -> (Double -> Double -> Double) -> (String, Expr)
funcRRR name f = builtinFunction name 2 $ F_RRR name f

funcZR :: String -> (Integer -> Double) -> (String, Expr)
funcZR name f = builtinFunction name 1 $ F_ZR name f

funcRB :: String -> (Double -> Bool) -> (String, Expr)
funcRB name f = builtinFunction name 1 $ F_RB name f

funcPut :: String -> String -> (String, Expr)
funcPut name f = builtinFunction name 0 $ Put name f

funcBye :: String -> (String, Expr)
funcBye name = builtinFunction name 0 $ Bye name
--funcInt name = builtinFunction name 1 $ F_RZ name id floor floor
--funcFloat name = builtinFunction name 1 $ F_RR name id
--funcRat name = builtinFunction name 1 $ F_RQ name id id toRational

mantissa :: Double -> Double
mantissa x = x / (2.0 ** fromIntegral (exponent x))

exponent' :: Double -> Integer
exponent' = ceiling . logBase 2

bye :: String
bye = undefined -- TODO : comment sortir ??? utiliser un code particulier

getState :: State -> String -> Int -> Maybe Expr
getState (_, global, local) name arity = get (local++global++builtin)
    where
        get [] = Nothing
        get ((name', val@(Def _ xs _)) : defs)
            | name' == name && length xs == arity = Just val
            | otherwise = get defs
        get ((name', val) : defs)
            | name' == name && arity == 0 = Just val
            | otherwise = get defs

setState :: State -> String -> Expr -> State
setState (conf, global, local@[]) name value = (conf, (name, value):global, local)
setState (conf, global, local) name value = (conf, global, (name, value):local)

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

pp' Nop = ""

pp' (E err) = err
pp' x = show x
