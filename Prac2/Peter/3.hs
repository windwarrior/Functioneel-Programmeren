import FPPrac
import Prelude (Bool)
import Prelude (Int)

erastote = [2..]

isPrime :: Int -> Bool
isPrime n
	| n<2 									= False
	| head (getDelers n) == n 		= True
	| otherwise 							= False
	
isMultiple :: Int -> Int -> Bool
isMultiple n x = ((mod n x )== 0)

getPrimesAmount :: Number -> [Int]
getPrimesAmount n = take n (filter isPrime [2..])

getPrimesSmallerThan :: Int -> [Int]
getPrimesSmallerThan n = filter isPrime [2..(n-1)]

getDelers :: Int -> [Int]
getDelers n = filter (isMultiple n) [2..n]