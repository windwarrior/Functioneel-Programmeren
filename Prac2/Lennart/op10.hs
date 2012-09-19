import FPPrac
import Prelude (Int)

mergesort :: [Int] -> [Int]
mergesort [] = []
mergesort (x:[]) = [x] -- blijkbaar gaat ie anders altijd door :O --
mergesort x = merge (mergesort x1) (mergesort x2)
    where x1 = take (ceiling ((length x)/2)) x
          x2 = drop (ceiling ((length x)/2)) x
    
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge x [] = x
merge [] y = y
merge x y
    | (x !! 0) > (y !! 0) = (y !! 0) : (merge x (drop 1 y))
    | (x !! 0) <= (y !! 0) = (x !! 0) : (merge (drop 1 x) y)
