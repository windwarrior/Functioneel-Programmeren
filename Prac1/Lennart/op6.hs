import FPPrac
import Data.Int
import Prelude (Bool)
mylength :: [a] -> Int
mylength [] = 0
mylength (x:xs) = 1 + mylength xs

mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + mysum xs

myreverse :: [a] -> [a]
myreverse [] = []
myreverse (x:xs) = myreverse xs ++ (x : [])

mytake :: [a] -> Int -> [a]
mytake [] n = []
mytake (x:xs) 0 = []
mytake (x:xs) n = x :  mytake xs (n-1)

myelem :: [Int] -> Int -> Bool
myelem [] a = False
myelem (x:xs) a 
    | x == a = True
    | otherwise = myelem xs a

myconcat :: [[a]] -> [a]
myconcat [] = []
myconcat (x:xs) = x ++ myconcat xs

mymaximum :: [Int] -> Int
mymaximum [] = 0
mymaximum (x:xs) 
    | x > mymaximum xs = x
    | otherwise = mymaximum xs

myzip :: ([a],[a]) -> [(a,a)]
myzip (x:xs, []) = []
myzip ([], y:ys) = []
myzip (x:xs, y:ys) = (x,y) : myzip (xs, ys)
