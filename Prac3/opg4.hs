import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

t_insert :: Tree1c -> Number -> Tree1c
t_insert Leaf1c n = Node1c n Leaf1c Leaf1c
t_insert (Node1c num child1 child2) n
	| n > num 	= Node1c num child1 (t_insert child2 n)
	| otherwise 	= Node1c num (t_insert child1 n) child2

-- --------------------------------------------------------------------------------------------------------

makeTree :: [Number] -> Tree1c
makeTree xs = foldl t_insert Leaf1c xs

makeTreeRec :: [Number] -> Tree1c
makeTreeRec [] = Leaf1c
makeTreeRec (x:xs) = t_insert (makeTreeRec xs) x

-- --------------------------------------------------------------------------------------------------------

makeList :: Tree1c -> [Number]
makeList Leaf1c = []
makeList (Node1c num child1 child2) = (makeList child1) ++ [num] ++ (makeList child2)

-- --------------------------------------------------------------------------------------------------------

sortList :: [Number] -> [Number]
sortList xs = makeList (makeTree xs)

-- --------------------------------------------------------------------------------------------------------

sortTree :: Tree1c -> Tree1c
sortTree tree = makeTreeRec (makeList tree)