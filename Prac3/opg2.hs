import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

sumTree :: Tree1a -> Number -> Tree1a
sumTree (Leaf1a num) n = Leaf1a (num+n)
sumTree (Node1a num child1 child2) n = Node1a (num+n) (sumTree child1 n) (sumTree child2 n)

-- --------------------------------------------------------------------------------------------------------

sqrTree :: Tree1a -> Tree1a
sqrTree (Leaf1a num) = Leaf1a (num*num)
sqrTree (Node1a num child1 child2) = Node1a (num*num) (sqrTree child1) (sqrTree child2)

-- --------------------------------------------------------------------------------------------------------

mapTree :: (Number -> Number) -> Tree1a -> Tree1a
mapTree f (Leaf1a num) = Leaf1a (f num)
mapTree f (Node1a num child1 child2) = Node1a (f num) (mapTree f child1) (mapTree f child2)

-- --------------------------------------------------------------------------------------------------------

telopNode :: Tree1b -> Tree1a
telopNode (Leaf1b (num1,num2)) = Leaf1a (num1+num2)
telopNode (Node1b (num1,num2) child1 child2) = Node1a (num1+num2) (telopNode child1) (telopNode child2)

-- --------------------------------------------------------------------------------------------------------

mapTree1b :: ((Number,Number) -> Number) -> Tree1b -> Tree1a
mapTree1b f (Leaf1b (num1,num2)) = Leaf1a (f (num1,num2))
mapTree1b f (Node1b (num1,num2) child1 child2) = Node1a (f (num1,num2)) (mapTree1b f child1) (mapTree1b f child2)

showPlus 	= showTree (pp1a (mapTree1b (\(num1,num2) -> (num1+num2)) example1b))
showMin 	= showTree (pp1a (mapTree1b (\(num1,num2) -> (num1-num2)) example1b))
showMul 	= showTree (pp1a (mapTree1b (\(num1,num2) -> (num1*num2)) example1b))