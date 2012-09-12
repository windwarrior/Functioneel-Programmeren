import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

binspiegel :: Tree1a -> Tree1a
binspiegel (Leaf1a num) = Leaf1a num
binspiegel (Node1a num child1 child2) = Node1a num (binspiegel child2) (binspiegel child1)

-- --------------------------------------------------------------------------------------------------------

binspiegel1d :: Tree1d -> Tree1d
binspiegel1d (Leaf1d (num1,num2)) = Leaf1d (num2,num1)
binspiegel1d (Node1d (num1,num2) child1 child2) = Node1d (num2,num1) (binspiegel1d child2) (binspiegel1d child1)