import FPPrac
import Prelude (Bool)
import Prelude (Int)

allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x:xs)
	| xs == [] 		= True
	| otherwise 	= (x == (head xs)) && allEqual xs

isRR :: [Int] -> Bool
isRR [] = True
isRR (x:xs) 
	| xs == [] 	 = True
	| tail xs == [] = True
	| otherwise 	 = (second - first)==(third - second)  && isRR xs
	where 
		third = (head (tail xs))
		second = (head xs)
		first = x