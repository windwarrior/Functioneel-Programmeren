import FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char


list = [2..]

zeef :: [Int] -> [Int]
zeef [] = []
zeef (x:xs) = x : zeef (filter (\a -> a `mod` x > 0) xs )

isPrime :: Int -> Bool
isPrime x =  l !! ((length l) - 1) == x
    where l = zeef [2..x]

getPrimes :: Number -> [Int]
getPrimes x = take x (zeef list)

getPrimesSmallerThen :: Int -> [Int]
getPrimesSmallerThen n = zeef [2..(n-1)]

delers :: Int -> [Int]
delers n = filter (\a -> n `mod` a == 0) [1..n]

isPrimeD :: Int -> Bool
isPrimeD n = length (delers n) == 2
