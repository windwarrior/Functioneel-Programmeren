import FPPrac
import Prelude (Int)

r :: Number -> Number -> [Number]
r s v = (s : r (s+v) v)

--r1 :: Number -> Number -> Int -> Number
r1 s v n = (r s v) !! (n-1)

totaal i j s v = sum (drop (i-1) (take j list)) 
	where list = r s v