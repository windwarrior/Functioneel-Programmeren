import FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char
import Data.List (subsequences)

deellijst :: [Number] -> [Number] -> Bool
deellijst x y =  x `elem` [take (yl - (i+j)) (drop j y) | j <- [0..yl-1], i <- [j..yl]]
    where
        yl = length y
{-Dit was het origineel, toen kwam ik achter het bestaan van subsequences dus dit kan ook. Helaas krijg ik de duplicate lege lijsten er niet uit :(
--deellijst x y = x `elem` subsequences y
-}

sublijst :: [Number] -> [Number] -> Bool
sublijst [] [] = True
sublijst [] _ = True
sublijst _ [] = False
sublijst (x:xs) (y:ys) 
	| x == y 		= sublijst xs ys
	| otherwise 	= sublijst (x:xs) ys
