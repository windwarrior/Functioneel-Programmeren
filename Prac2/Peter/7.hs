import FPPrac
import Prelude (Bool)
import Prelude (Int)

bubble :: [Int] -> [Int]
bubble [] = []
bubble (x:xs)
	| xs == [] 		= [x]
	| x > head xs = head xs : (bubble (x : tail xs))
	| otherwise 	= x : bubble xs
	
bsort :: [Int] -> [Int]
bsort xs 
	| bubble xs == xs 	= xs
	| otherwise			= bsort (bubble xs)