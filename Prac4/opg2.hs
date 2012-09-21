import FPPrac
import TypesBinTree
import Data.Char (isNumber)
-- atoi :: String -> Number (integral)
-- atof :: String -> Number (floating)
import Data.Either
data Types = E | G | O | H
data Operator = Plus | Min | Multiply | Divide | Power
type EitherNumberChar = Either Number Char
parseOperator :: Char -> Operator -- (BinTree Operator Number, String)
parseOperator x
    | x == '+' = Plus 
    | x == '-' = Min
    | x == '*' = Multiply
    | x == '\\' = Divide
    | x == '^' = Power
{-}
parse :: String -> (BinTree Operator Number, String)
parse (x:xs)
    | x == '(' = parseExp xs
    | isNumber x = BinLeaf (atoi x)
    | otherwise = error ("invalid syntax")-}

parseExp :: Types -> String -> (BinTree Operator Number, String)
parseExp E (x:xs)
    {-| x == ')' = -- Einde expressie-}
    | x == '(' = (BinNode oper t1 t2 , r4)
    | isNumber x = parseExp G (x:xs)
    | otherwise = error "dit werkt niet"
    where
        (t1, r1) = parseExp E xs
        (BinNode oper _ _ , r2) = parseExp O r1
        (BinNode _ _ t2, r3) = parseExp E r2
        (BinNode _ _ _, r4) = parseExp H r3

parseExp O (x:xs) = (BinNode (parseOperator x) (BinLeaf 0) (BinLeaf 0), xs)

parseExp G (x:xs) = (BinNode Plus (BinLeaf (atoi (x:""))) (BinLeaf 0), xs)

parseExp H (x:xs) = (BinNode Plus (BinLeaf 0) (BinLeaf 0), xs)

-- ---------------------------------------------------------------------------

parseExpVar :: Types -> String -> (BinTree Operator EitherNumberChar, String)
parseExpVar E (x:xs)
    {-| x == ')' = -- Einde expressie-}
    | x == '(' = (BinNode oper t1 t2 , r4)
    | isNumber x || x `elem` ['a'..'Z'] = parseExpVar G (x:xs)
    | otherwise = error "dit werkt niet"
    where
        (t1, r1) = parseExpVar E xs
        (BinNode oper _ _ , r2) = parseExpVar O r1
        (BinNode _ _ t2, r3) = parseExpVar E r2
        (BinNode _ _ _, r4) = parseExpVar H r3

parseExpVar O (x:xs) = (BinNode (parseOperator x) (BinLeaf (Left 0)) (BinLeaf (Left 0)), xs)

parseExpVar G (x:xs) 
    | isNumber x = (BinNode Plus (BinLeaf (Left (atoi (x:"")))) (BinLeaf (Left 0)), xs)
    | otherwise = (BinNode Plus (BinLeaf (Right x)) (BinLeaf (Left 0)), xs)

parseExpVar H (x:xs) = (BinNode Plus (BinLeaf (Left 0)) (BinLeaf (Left 0)), xs)
