import FPPrac
import Prelude (Bool)
import Data.Int

equalLong :: [[Int]] -> Bool
equalLong [] = True
equalLong (x:xs)
    | xs == [] = True
    | otherwise = length x == length (head xs) && equalLong xs

totalRows :: [[Int]] -> [Int]
totalRows [] = []
totalRows (x:xs) = sum(x) : totalRows(xs)

transpose :: [[Int]] -> [[Int]] 
transpose ([]:_) = []
transpose m = map head m : transpose (map tail m)


totalColumn :: [[Int]] -> [Int]
totalColumn ([]:_) = []
totalColumn m = sum (map head m) : totalColumn (map tail m)
