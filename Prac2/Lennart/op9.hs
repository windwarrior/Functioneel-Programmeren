import Prelude
import Data.List

insertionsort :: [Int] -> [Int]
insertionsort [] = []
insertionsort (x:[]) = [x]
insertionsort (x:xs) = insert x (insertionsort xs)

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldr :: (b -> a -> a) -> a -> [b] -> a
insert :: a -> [a] -> [a]

insertsort2 = foldr insert []
