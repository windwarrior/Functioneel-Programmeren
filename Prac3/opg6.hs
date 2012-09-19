import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

totDiepte :: Tree1a -> Number -> Tree1a
totDiepte (Leaf1a l) _ = num
totDiepte (Node1a n ch1 ch2) x 
    | x == 0 = Leaf1a n
    | otherwise = Node1a n (totDiepte ch1 (x-1)) (totDiepte ch2 (x-1))
