import FPPrac
import Prelude (Int)

quicksort :: [Int] -> [Int]
quicksort [] = []
quicksort (x:xs) =  quicksort([i | i <- xs, i < x]) ++ (x : quicksort([i | i <- xs, i == x])) ++ quicksort([i | i <- xs, i > x])

