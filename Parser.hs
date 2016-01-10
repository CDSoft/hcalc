module Parser where

import Expression

import Data.Char
import Data.List

type Parser t = String -> [(t, String)]

parse :: String -> Expr
parse input = if null left then expression else E $ "Error near " ++ take 16 left
    where
        input' = spaces input
        (expression, left) = best (stats input' ++ [(E "syntax error", input')])

        best (e:es) = get e es -- trace (show (e:es)) (get e es)
            where
                get e1@(_, "") _ = e1
                get _ ((e2@(_, "")):_) = e2
                get e1@(_, l1) ((e2@(_, l2)):es') = get (if length l2 < length l1 then e2 else e1) es'
                get e1 [] = e1
        best [] = error "best must be called with a non empty list"

        spaces :: String -> String
        spaces (c:s) | isSpace c = spaces s
                     | c == '#' = comment s
                     | otherwise = c:s
        spaces [] = []
        comment (c:s) | c `elem` "\r\n" = spaces s
                       | otherwise = comment s
        comment [] = []

        seq1 f p1 s = [ (f e1, s1) | (e1, s1) <- p1 s ]
        seq2 f p1 p2 s = [ (f e1 e2, s2) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1 ]
        seq3 f p1 p2 p3 s = [ (f e1 e2 e3, s3) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2 ]
        seq4 f p1 p2 p3 p4 s = [ (f e1 e2 e3 e4, s4) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2, (e4, s4) <- p4 s3 ]
        seq5 f p1 p2 p3 p4 p5 s = [ (f e1 e2 e3 e4 e5, s5) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2, (e4, s4) <- p4 s3, (e5, s5) <- p5 s4 ]
        seq6 f p1 p2 p3 p4 p5 p6 s = [ (f e1 e2 e3 e4 e5 e6, s6) | (e1, s1) <- p1 s, (e2, s2) <- p2 s1, (e3, s3) <- p3 s2, (e4, s4) <- p4 s3, (e5, s5) <- p5 s4, (e6, s6) <- p6 s5 ]

        p1 ||| p2 = \s -> p1 s ++ p2 s

        nothing x s = [(x, s)]

        stats :: Parser Expr -- String -> [(Expr, String)]
        --stats =
        --    seq2 (\s1 f -> f s1) stat stats'
        --    ||| \s -> [ (Nop, spaces s)]
        --    where
        --        stats' =
        --            seq3 (\_ s2 f s1 -> f (Seq s1 s2)) (tok ";" ()) stat stats'
        --            ||| tok ";" id
        --            ||| (\s -> [(id, s)])
        stats = -- chainBinOps stat (tok ";" Seq)
            seq2 const (chainBinOps stat (tok ";" Seq)) (tok ";" () ||| nothing ())
            ||| tok ";" Nop
            ||| nothing Nop

        stat :: Parser Expr -- String -> [(Expr, String)]
        stat =
            seq6 (\f _ xs _ _ y -> Def f xs y)
                 ident (tok "(" ()) idents (tok ")" ()) (tok "=" ()) expr
            ||| seq3 (\f _ y -> Def f [] y)
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

        idents :: Parser [String] -- String -> [([String], String)]
        idents =
            seq3 (\x _ xs -> x : xs) ident (tok "," ()) idents
            ||| seq1 (:[]) ident
            ||| nothing []

        exprs :: Parser [Expr]
        exprs =
            seq3 (\x _ xs -> x : xs) expr (tok "," ()) exprs
            ||| seq1 (:[]) expr
            ||| nothing []

        tok t e s = [ (e, spaces (drop (length t) s)) | t `isPrefixOf` s ]

        expr = ternary ||| condition

        ternary = seq5 (\c _ e1 _ e2 -> Tern c e1 e2)
                       condition (tok "?" ()) expr (tok ":" ()) expr

        -- pattern for chained binary operators
        -- P -> Q (op Q)*
        -- same as
        -- P -> Q P'
        -- P' -> op Q P'

        chainBinOps q ops = seq2 (\e e' -> e' e) q p'
            where
                p' = seq3 (\op e2 e' e1 -> e' (op e1 e2)) ops q p' ||| nothing id

        -- pattern for chained unary operators
        -- P -> op* Q
        -- same as
        -- P -> op P | Q

        chainUnOps ops p q = seq2 (\op e -> op e) ops p ||| q

        condition = condOr
        condOr = chainBinOps condAnd (tok "or" Or ||| tok "xor" Xor)
        condAnd = chainBinOps condNot (tok "and" And)

        condNot = chainUnOps (tok "not" Not) condNot comparison

        -- pattern for chained relational operators
        -- P -> Q op Q op Q ... => Q op Q and Q op Q ...
        -- same as
        -- P -> Q P'
        -- P' -> op Q P'

        --chainRelOps q ops = seq2 (\e1 e' -> e' e1) q p'
        --    where
        --        p' = seq3 (\op e2 e' e1 -> And (op e1 e2) (e' e2)) ops q p' ||| nothing id

        comparison = chainBinOps bitwise (tok "<=" Le ||| tok "<" Lt ||| tok ">=" Ge ||| tok ">" Gt ||| tok "==" Eq ||| tok "!=" Ne)

        bitwise = bitwiseOr
        bitwiseOr = chainBinOps bitwiseXor (tok "|" BitOr)
        bitwiseXor = chainBinOps bitwiseAnd (tok "^" BitXor)
        bitwiseAnd = chainBinOps bitshift (tok "&" BitAnd)
        bitshift = chainBinOps add (tok "<<" LShift ||| tok ">>" RShift)

        add = chainBinOps mul (tok "+" Add ||| tok "-" Sub)
        mul = chainBinOps unary (tok "*" Mul ||| tok "/" Div ||| tok "//" Quot ||| tok "%" Mod)
        unary = chainUnOps (tok "+" Pos ||| tok "-" Neg ||| tok "~" BitNeg) unary pow

        pow = seq3 (\x op y -> op x y) atom (tok "**" Pow) pow ||| atom

        atom =
            seq3 (\_ x _ -> x) (tok "(" ()) stats (tok ")" ())
            ||| int ||| intHex ||| intOct ||| intBin
            ||| flt
            ||| str
            ||| function

        int s = [(Z (read ds), spaces s') | (not . null) ds]
            where (ds, s') = span isDigit s

        intHex ('0':x:s) = [(Seq (SetHex Nothing) (Z (read ("0x"++ds))), spaces s') | x `elem` "xX", (not . null) ds]
            where (ds, s') = span isHexDigit s
        intHex _ = []

        intOct ('0':o:s) = [(Seq (SetOct Nothing) (Z (read ("0o"++ds))), spaces s') | o `elem` "oO", (not . null) ds]
            where (ds, s') = span (`elem` "01234567") s
        intOct _ = []

        intBin ('0':b:s) = [(Seq (SetBin Nothing) (Z (readBin 0 ds)), spaces s') | b `elem` "bB", (not . null) ds]
            where (ds, s') = span (`elem` "01") s
                  readBin n [] = n
                  readBin n (x:xs) = readBin (2*n+x') xs where x' = if x == '0' then 0 else 1
        intBin _ = []

        flt s = case reads s of
                    [(x, s')] -> [(R x, spaces s')]
                    _ -> []

        str ('"':s) = [(S s'', cs'')]
            where
                str' ('"':cs) = ("", spaces cs)
                str' ('\\':c:cs) = (c:cs', s') where (cs', s') = str' cs
                str' (c:cs) = (c:cs', s') where (cs', s') = str' cs
                str' "" = ("", "")
                (s'', cs'') = str' s
        str _ = []

        function =
            seq4 (\name _ args _ -> F name args) ident (tok "(" ()) exprs (tok ")" ())
            ||| seq1 (\name -> F name []) ident
