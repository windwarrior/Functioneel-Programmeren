import FPPrac
import Prelude (Bool)
import Prelude (Int)

qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:[]) = [x]
qsort (x:xs) = smaller ++ same ++ greater
	where
		smaller  = filter (<x) (qsort xs)
		same 	=  x: (filter (==x) (qsort xs))
		greater 	= 	filter (>x) (qsort xs)