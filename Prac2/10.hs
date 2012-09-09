import FPPrac
import Prelude (Bool)
import Prelude (Int)
import Data.List

merget :: [Int] -> [Int] -> [Int]
merget xs [] = xs
merget (xs) (y:ys) = merget (insert y xs) ys

msort :: [Int] -> [Int]
msort [] 		= []
msort (x:[]) 	= [x]
msort xs 		= merget split1 split2
	where
		take 		= FPPrac.take
		drop 		= FPPrac.drop
		length 	= FPPrac.length
		split1 	= msort (take (ceiling ((length xs)/2)) xs)
		split2 	= msort (drop (ceiling ((length xs)/2)) xs)