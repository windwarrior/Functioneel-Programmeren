import Prelude
import Data.List

insertionsort :: [Int] -> [Int]
insertionsort [] = []
insertionsort (x:[]) = [x]
insertionsort (x:xs) = insert x (insertionsort xs)
