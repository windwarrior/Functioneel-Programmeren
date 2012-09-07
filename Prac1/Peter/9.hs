import FPPrac
import Prelude (Bool)
import Prelude (Int)

equallyLongRows :: [[Int]] -> Bool
equallyLongRows [] = True
equallyLongRows (x:xs) 
	| xs == [] 		= True
	| otherwise		= length x == length (head xs) && equallyLongRows xs

sumColumns1 :: [[Int]] -> [Int]
sumColumns1 [] 		= repeat 0
sumColumns1 (x:xs) = zipWith (+) x (sumColumns1 xs)

sumColumns2 ([]:_)  = []
sumColumns2 m =  sum (map head m)  : sumColumns2 (map tail m)

sumRows :: [[Int]] -> [Int]
sumRows [] = []
sumRows (x:xs) = sum x : (sumRows xs)

transpose :: [[Int]] -> [[Int]]
transpose ([]:_) = []
transpose m 		= map head m : transpose (map tail m)