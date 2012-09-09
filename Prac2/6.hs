import FPPrac
import Prelude (Bool)
import Prelude (Int)

deellijst :: [Int] -> [Int] -> Bool
deellijst x y = not ( (filter (==x) [ a | b <- [0..(length y)-(length x)], a <- [take (length x) (drop b y)]]) == [] )

sublijst [] [] = True
sublijst [] _ = True
sublijst _ [] = False
sublijst (x:xs) (y:ys) 
	| x == y 		= sublijst xs ys
	| otherwise 	= sublijst (x:xs) ys