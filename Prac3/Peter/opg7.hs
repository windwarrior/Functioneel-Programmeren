import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

vervang :: Tree1a -> String -> Number -> Tree1a
vervang (Leaf1a num) "" n = Leaf1a n
vervang (Node1a num child1 child2) "" n = Node1a n child1 child2
vervang (Node1a num child1 child2) (p:ps) n
	| p=='l'	= Node1a num (vervang child1 ps n) child2
	| p=='r'	= Node1a num child1 (vervang child2 ps n) 

-- --------------------------------------------------------------------------------------------------------

subboom :: Tree1a -> String -> Tree1a
subboom tree "" = tree
subboom (Leaf1a num) _ = error ("wrong path")
subboom (Node1a num child1 child2) (p:ps)
	| p=='l'	= subboom child1 ps
	| p=='r'	= subboom child2 ps

-- --------------------------------------------------------------------------------------------------------

