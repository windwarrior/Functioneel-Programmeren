import FPPrac
import Prelude (Bool)
import Prelude (Int)

isPyth :: (Int,Int,Int) -> Bool
isPyth (a,b,c) = a^2 + b^2 == c^2

pyth :: Int -> [(Int,Int,Int)]
pyth n = filter isPyth [(a,b,c) | a <- [0..n], b <- [0..n], c <- [0..n] ]

pyth' :: Int -> [(Int,Int,Int)]
pyth' n = filter isPyth [(a,b,c) | a <- [0..n], b <- [a..n], c <- [0..n] ]