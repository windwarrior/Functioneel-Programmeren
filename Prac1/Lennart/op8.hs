import FPPrac
import Prelude (Bool)
import Data.Int

allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x:xs) 
    | xs == [] = True    
    | otherwise = x == head xs && allEqual xs

-- opzich klopt ie, maar snap hem nog niet helemaal --
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
