import FPPrac
import TypesBinTree
import Data.Char (isNumber)
-- atoi :: String -> Number (integral)
-- atof :: String -> Number (floating)
import Data.Either
data Types = E | G | O | H
data Operator = Plus | Min | Multiply | Divide | Power
	deriving (Show, Eq)
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
        (t2, r3) = parseExp E r2
        (_, r4) = parseExp H r3

parseExp O (x:xs) = (BinNode (parseOperator x) (BinLeaf 0) (BinLeaf 0), xs)

parseExp G (x:xs) = (BinLeaf (atoi (x:"")), xs)

parseExp H (x:xs) = (BinNode Plus (BinLeaf 0) (BinLeaf 0), xs)

-- ---------------------------------------------------------------------------

parseExpVar :: Types -> String -> (BinTree Operator EitherNumberChar, String)
parseExpVar E (x:xs)
    {-| x == ')' = -- Einde expressie-}
    | x == '(' = (BinNode oper t1 t2 , r4)
    | isNumber x || x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = parseExpVar G (x:xs)
    | otherwise = error "dit werkt niet"
    where
        (t1, r1) = parseExpVar E xs
        (BinNode oper _ _ , r2) = parseExpVar O r1
        (t2, r3) = parseExpVar E r2
        (BinNode _ _ _, r4) = parseExpVar H r3

parseExpVar O (x:xs) = (BinNode (parseOperator x) (BinLeaf (Left 0)) (BinLeaf (Left 0)), xs)

parseExpVar G (x:xs) 
    | isNumber x = (BinNode Plus (BinLeaf (Left (atoi (x:"")))) (BinLeaf (Left 0)), xs)
    | otherwise = (BinNode Plus (BinLeaf (Right x)) (BinLeaf (Left 0)), xs)

parseExpVar H (x:xs) = (BinNode Plus (BinLeaf (Left 0)) (BinLeaf (Left 0)), xs)

-- ---------------------------------------------------------------------------
type EitherNumberString = Either Number String
parseExpSpaceMulti :: Types -> String -> (BinTree Operator EitherNumberString, String)
parseExpSpaceMulti E (x:xs)
    {-| x == ')' = -- Einde expressie-}
    | x == '(' = (BinNode oper t1 t2 , r4)
	| x == ' ' = parseExpSpaceMulti E xs
    | isNumber x || x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = parseExpSpaceMulti G (x:xs)
    | otherwise = error "dit werkt niet"
    where
        (t1, r1) = parseExpSpaceMulti E xs
        (BinNode oper _ _ , r2) = parseExpSpaceMulti O r1
        (t2, r3) = parseExpSpaceMulti E r2
        (BinNode _ _ _, r4) = parseExpSpaceMulti H r3

parseExpSpaceMulti O (x:xs)
	| x == ' ' = parseExpSpaceMulti O xs
	| otherwise = (BinNode (parseOperator x) (BinLeaf (Left 0)) (BinLeaf (Left 0)), xs)

parseExpSpaceMulti G (x:xs) 
	| x == ' ' = parseExpSpaceMulti G xs
    | isNumber x = (BinLeaf (Left (atoi (number))), (drop (length number) (x:xs)))
    | x `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = (BinLeaf (Right number), (drop (length number) (x:xs)))
	| otherwise = error "dit is stuk"
		where 
			number = (parseNumber (x:xs) "")
			
parseExpSpaceMulti H (x:xs) 
	| x == ' ' = parseExpSpaceMulti H xs
	| otherwise = (BinNode Plus (BinLeaf (Left 0)) (BinLeaf (Left 0)), xs)

data State = Number | Variable | Other
	deriving (Show, Eq)

-- Parsebare string -> RestString -> (ummerString -> RestString)
-- Ik weet niet of het mag met zo'n hulpfunctie, maar het werkt :>
parseNumber :: String -> String -> String
parseNumber "" y = y
parseNumber (x) (y)
	| (numberFSM Number x) == Other && (numberFSM Variable x) == Other  = parseNumber (take (length x - 1) x) ((drop (length x - 1) x) ++ y)
	| (numberFSM Number x) == Number = (x)
	| (numberFSM Variable x) == Variable = (x)

numberFSM :: State -> String -> State
numberFSM i [] = i
numberFSM Variable (x:xs)
	| x  `elem` ['a'..'z'] || x `elem` ['A'..'Z'] = numberFSM Variable xs
	| otherwise = numberFSM Other xs
numberFSM Number (x:xs) 
	| isNumber x  = numberFSM Number xs
	| otherwise = numberFSM Other xs
numberFSM _ (x:xs) = numberFSM Other xs

-- ---------------------------------------------------------------------------

evalExp1 :: String -> Number
evalExp1 x = evalTree (fst (parseExp E x))

evalTree :: BinTree Operator Number -> Number
evalTree (BinLeaf x) = x
evalTree (BinNode operator child1 child2)
	| operator == Plus     = (evalTree (child1)) + (evalTree (child2))
	| operator == Min      = (evalTree (child1)) - (evalTree (child2))
	| operator == Multiply = (evalTree (child1)) * (evalTree (child2))
	| operator == Divide   = (evalTree (child1)) / (evalTree (child2))
	| operator == Power    = (evalTree (child1)) ^ (evalTree (child2))

evalExp2 :: (Char -> Number) -> String -> Number
evalExp2 f x = evalTree postProcessed
	where 
		preProcessed = (fst (parseExpVar E x))
		postProcessed = (f preProcessed)
		

assign :: Char -> Number -> (BinTree Operator EitherNumberChar -> BinTree Operator EitherNumberChar)
assign c n (BinNode op ch1 ch2) = (BinNode op (assign(c n ch1)) (assign(c n ch2)))
assign c n (BinLeaf char)
	| c == char = (BinLeaf Left n)
	| otherwise = (BinLeaf Right char)
