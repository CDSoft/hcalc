{- Handy Calc
Copyright (C) 2016, 2017 Christophe Delord
http://cdsoft.fr/hcalc

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

{- The module Parser parses strings and produces expressions of type Expr
-}

module Parser(parse) where

import Expression

import Data.Char
import Data.List

-- a parser is a function that takes a string and returns a list.
-- Each item is a possible way to parse the string, i.e. an expression
-- and the remaning string
type Parser t = String -> [(t, String)]

parse :: String -> Expr
parse input = let input' = spaces input in
              case best (stats input' ++ [(E "syntax error", input')]) of
                  (expression, []) -> expression
                  (_, left) -> E $ "Error near " ++ take 16 left

best :: [(Expr, String)] -> (Expr, String)
best (e:es) = get e es
    where
        -- nothing left -> this is the best candidate
        get e1@(_, "") _ = e1
        get _ ((e2@(_, "")):_) = e2
        -- take the candidate that has parsed the longest string
        get e1@(_, l1) ((e2@(_, l2)):es') = get (if length l2 < length l1 then e2 else e1) es'
        -- no more candidates
        get e1 [] = e1
best [] = error "best must be called with a non empty list"

-- spaces removes spaces and comments at the beginning of a string
spaces :: String -> String
spaces (c:s) | isSpace c = spaces s
             | c == '#' = comment s
             | otherwise = c:s
spaces [] = []

-- comment remove a comment, i.e. chars upto the end of the line
comment :: String -> String
comment (c:s) | c `elem` "\r\n" = spaces s
               | otherwise = comment s
comment [] = []

seq1 :: (t1->t) -> Parser t1 -> Parser t
seq1 f p1 s = [ (f e1, s1) | (e1, s1) <- p1 s ]

seq2 :: (t1->t2->t) -> Parser t1 -> Parser t2 -> Parser t
seq2 f p1 p2 s = [ (f e1 e2, s2) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1 ]

seq3 :: (t1->t2->t3->t) -> Parser t1 -> Parser t2 -> Parser t3 -> Parser t
seq3 f p1 p2 p3 s = [ (f e1 e2 e3, s3) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2 ]

seq4 :: (t1->t2->t3->t4->t) -> Parser t1 -> Parser t2 -> Parser t3 -> Parser t4 -> Parser t
seq4 f p1 p2 p3 p4 s = [ (f e1 e2 e3 e4, s4) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2, (e4, s4) <- p4 s3 ]

seq5 :: (t1->t2->t3->t4->t5->t) -> Parser t1 -> Parser t2 -> Parser t3 -> Parser t4 -> Parser t5 -> Parser t
seq5 f p1 p2 p3 p4 p5 s = [ (f e1 e2 e3 e4 e5, s5) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2, (e4, s4) <- p4 s3, (e5, s5) <- p5 s4 ]

seq6 :: (t1->t2->t3->t4->t5->t6->t) -> Parser t1 -> Parser t2 -> Parser t3 -> Parser t4 -> Parser t5 -> Parser t6 -> Parser t
seq6 f p1 p2 p3 p4 p5 p6 s = [ (f e1 e2 e3 e4 e5 e6, s6) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2, (e4, s4) <- p4 s3, (e5, s5) <- p5 s4, (e6, s6) <- p6 s5 ]

(|||) :: Parser t -> Parser t -> Parser t
p1 ||| p2 = \s -> longuest (p1 s ++ p2 s)

longuest :: [(a, String)] -> [(a, String)]
longuest [] = []
longuest xs = [minimumBy (\(_, s1) (_, s2) -> compare (length s1) (length s2)) xs]

nothing :: t -> Parser t
nothing x s = [(x, s)]

-- stats -> stat (; stat)* ;?
--       |  ;
--       |  {}
stats :: Parser Expr
stats =
    seq2 const (chainBinOps stat (tok ";" Seq)) (tok ";" () ||| nothing ())
    ||| tok ";" None
    ||| nothing None

-- stat -> ident "(" idents ")" "=" expr
--      |  ident "=" expr
--      |  expr
stat :: Parser Expr
stat =
    seq6 (\f _ xs _ _ y -> Def f xs [] y)
         ident (tok "(" ()) idents (tok ")" ()) (tok "=" ()) expr
    ||| seq3 (\f _ y -> Def f [] [] y)
             ident (tok "=" ()) expr
    ||| expr

ident :: Parser String
ident s@(c:_)
    | isAlpha c || c == '_' = case span (\c' -> isAlphaNum c' || c' == '_') s of
        ("", _) -> []
        (name, s1) -> [(name, spaces s1)]
    | otherwise = []
ident [] = []

--idents s = [ (x:xs, s9) | (x, s1) <- ident s, (xs, s9) <- idents s1] ++ nothing [] s
--exprs s = [ (x:xs, s9) | (x, s1) <- expr s, (xs, s9) <- exprs s1] ++ nothing [] s

-- idents -> ident "," idents
--        |  ident
--        |  {}
idents :: Parser [String] -- String -> [([String], String)]
idents =
    seq3 (\x _ xs -> x : xs) ident (tok "," ()) idents
    ||| seq1 (:[]) ident
    ||| nothing []

-- exprs -> expr "," exprs
--       |  expr
--       |  {}
exprs :: Parser [Expr]
exprs =
    seq3 (\x _ xs -> x : xs) expr (tok "," ()) exprs
    ||| seq1 (:[]) expr
    ||| nothing []

tok :: String -> t -> Parser t
tok t e s = [ (e, spaces (drop (length t) s)) | t `isPrefixOf` s ]

-- expr -> ternary | condition
expr :: Parser Expr
expr = ternary ||| condition

-- ternary -> condition "?" expr ":" expr
ternary :: Parser Expr
ternary = seq5 (\c _ e1 _ e2 -> Tern c e1 e2)
               condition (tok "?" ()) expr (tok ":" ()) expr

-- pattern for chained binary operators
-- P -> Q (op Q)*
-- same as
-- P -> Q P'
-- P' -> op Q P' | {}
chainBinOps :: Parser Expr -> Parser (Expr->Expr->Expr) -> Parser Expr
chainBinOps q ops = seq2 (\e e' -> e' e) q p'
    where
        p' = seq3 (\op e2 e' e1 -> e' (op e1 e2)) ops q p' ||| nothing id

-- pattern for chained unary operators
-- P -> op* Q
-- same as
-- P -> op P | Q
chainUnOps :: Parser (Expr->Expr) -> Parser Expr -> Parser Expr -> Parser Expr
chainUnOps ops p q = seq2 (\op e -> op e) ops p ||| q

condition :: Parser Expr
condition = condOr

condOr :: Parser Expr
condOr = chainBinOps condAnd (tok "or" Or ||| tok "xor" Xor)

condAnd :: Parser Expr
condAnd = chainBinOps condNot (tok "and" And)

condNot :: Parser Expr
condNot = chainUnOps (tok "not" Not) condNot comparison

-- pattern for chained relational operators
-- P -> Q op Q op Q ... => Q op Q and Q op Q ...
-- same as
-- P -> Q P'
-- P' -> op Q P'

--chainRelOps q ops = seq2 (\e1 e' -> e' e1) q p'
--    where
--        p' = seq3 (\op e2 e' e1 -> And (op e1 e2) (e' e2)) ops q p' ||| nothing id

comparison :: Parser Expr
comparison = chainBinOps bitwise (   tok "<=" Le ||| tok "<" Lt
                                 ||| tok ">=" Ge ||| tok ">" Gt
                                 ||| tok "==" Eq ||| tok "!=" Ne)

bitwise :: Parser Expr
bitwise = bitwiseOr

bitwiseOr :: Parser Expr
bitwiseOr = chainBinOps bitwiseXor (tok "|" BitOr)

bitwiseXor :: Parser Expr
bitwiseXor = chainBinOps bitwiseAnd (tok "^" BitXor)

bitwiseAnd :: Parser Expr
bitwiseAnd = chainBinOps bitshift (tok "&" BitAnd)

bitshift :: Parser Expr
bitshift = chainBinOps add (tok "<<" LShift ||| tok ">>" RShift)

add :: Parser Expr
add = chainBinOps mul (tok "+" Add ||| tok "-" Sub)

mul :: Parser Expr
mul = chainBinOps unary (tok "*" Mul ||| tok "/" Div ||| tok "//" Quot ||| tok "%" Mod)

unary :: Parser Expr
unary = chainUnOps (tok "+" Pos ||| tok "-" Neg ||| tok "~" BitNeg) unary pow

pow :: Parser Expr
pow = seq3 (\x op y -> op x y) atom (tok "**" Pow) pow ||| atom

atom :: Parser Expr
atom =
    seq3 (\_ x _ -> x) (tok "(" ()) stats (tok ")" ())
    ||| int ||| intHex ||| intOct ||| intBin
    ||| flt
    ||| str
    ||| function

int :: Parser Expr
int s = [(Z (read ds), spaces s') | (not . null) ds]
    where (ds, s') = span isDigit s

intHex :: Parser Expr
intHex ('0':x:s) = [(Seq (SetHex Nothing) (Z (read ("0x"++ds))), spaces s') | x `elem` "xX", (not . null) ds]
    where (ds, s') = span isHexDigit s
intHex _ = []

intOct :: Parser Expr
intOct ('0':o:s) = [(Seq (SetOct Nothing) (Z (read ("0o"++ds))), spaces s') | o `elem` "oO", (not . null) ds]
    where (ds, s') = span (`elem` "01234567") s
intOct _ = []

intBin :: Parser Expr
intBin ('0':b:s) = [(Seq (SetBin Nothing) (Z (readBin 0 ds)), spaces s') | b `elem` "bB", (not . null) ds]
    where (ds, s') = span (`elem` "01") s
          readBin n [] = n
          readBin n (x:xs) = readBin (2*n+x') xs where x' = if x == '0' then 0 else 1
intBin _ = []

flt :: Parser Expr
flt s = case reads s of
            [(x, s')] -> [(R x, spaces s')]
            _ -> []

str :: Parser Expr
str ('"':s) = [(S s'', cs'')]
    where
        str' ('"':cs) = ("", spaces cs)
        str' ('\\':c:cs) = (c:cs', s') where (cs', s') = str' cs
        str' (c:cs) = (c:cs', s') where (cs', s') = str' cs
        str' "" = ("", "")
        (s'', cs'') = str' s
str _ = []

function :: Parser Expr
function =
    seq4 (\name _ args _ -> F name args) ident (tok "(" ()) exprs (tok ")" ())
    ||| seq1 (\name -> F name []) ident
