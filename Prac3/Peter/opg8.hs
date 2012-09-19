import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1
import Opg4

test :: Tree1c -> Bool
test tree = ((maxDepth tree) - (minDepth tree)) < 2

minDepth :: Tree1c -> Int
minDepth (Leaf1c) = 1
minDepth (Node1c num child1 child2) = 1+ minimum [(minDepth child1),(minDepth child2)]

maxDepth :: Tree1c -> Int
maxDepth (Leaf1c) = 1
maxDepth (Node1c num child1 child2) = 1+ maximum [(maxDepth child1),(maxDepth child2)]

-- --------------------------------------------------------------------------------------------------------

--balanceTree :: Tree1c -> Tree1c
balanceTree tree = makeBalancedTree list
	where
		list = makeList tree

makeBalancedTree [] = Leaf1c
makeBalancedTree (x:[]) = Node1c x Leaf1c Leaf1c
makeBalancedTree list = Node1c (list !! (half-1)) (makeBalancedTree (take (half-1) list)) (makeBalancedTree (drop half list))
	where
		half =  (ceiling ((length list)/2))