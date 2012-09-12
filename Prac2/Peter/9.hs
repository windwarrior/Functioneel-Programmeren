import FPPrac
import Prelude (Bool)
import Prelude (Int)
import Data.List

isort :: [Int] -> [Int]
isort [] = []
isort (x:[]) = [x]
isort (x:xs)  =  insert x (isort xs)