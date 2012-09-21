import FPPrac
import Prelude (String)
import Prelude (Int)
import Data.Char

--TODO het laatste element van bubble is al gesorteerd.
bubbleSort :: [Number] -> [Number]
bubbleSort x
    | x == bubble x = x
    | otherwise = bubbleSort (bubble x)

bubble :: [Number] -> [Number]
bubble [] = []
bubble (x:xs)
    | xs == [] = [x]
    | x > (xs !! 0) = (xs !! 0) : bubble (x : tail(xs))
    | x <= (xs !! 0) = x : bubble ((xs !! 0) : tail (xs))

