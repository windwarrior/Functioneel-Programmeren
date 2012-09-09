import FPPrac
import Prelude (Bool)
import Prelude (Int)
import Data.List

msort :: [Int] -> [Int]
msort [] 	= []
msort (x:[]) = [x] 
msort xs 	= [min] ++ ((msort(xs \\ [min,max])) ++ [max])
	where 
		min = minimum xs
		max = maximum xs