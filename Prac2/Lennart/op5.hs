import FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char

stijgend :: [Number] -> Bool
stijgend [] = True
stijgend (x:xs) 
    | xs == [] = True
    | otherwise = x < xs !! 0 && stijgend xs

zwakstijgend :: [Number] -> Bool
zwakstijgend x = stijgend (getGem x 0 0)

getGem :: [Number] -> Number -> Number -> [Number]
getGem [] total i = []
getGem (x:xs) total i = (total + x / (i + 1)) : getGem xs (total + x) (i + 1)
