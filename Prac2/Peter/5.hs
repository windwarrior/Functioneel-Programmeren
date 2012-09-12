import FPPrac
import Prelude (Bool)
import Prelude (Int)

stijgend :: [Int] -> Bool
stijgend [] = True
stijgend (x:xs) 
	| xs == [] 	= True
	| otherwise 	= x < head xs && stijgend xs

zwakStijgend :: [Int] -> Bool
zwakStijgend [] = True
zwakStijgend (x:xs) 
	| xs == [] 	= True
	| otherwise 	= x <= head xs && stijgend xs