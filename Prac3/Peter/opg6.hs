import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

totDiepte :: Number -> Tree1a -> Tree1a
totDiepte _ (Leaf1a num) = Leaf1a num 
totDiepte 1 (Node1a num child1 child2) = Leaf1a num
totDiepte n (Node1a num child1 child2) = (Node1a num (totDiepte (n-1) child1) (totDiepte (n-1) child2))