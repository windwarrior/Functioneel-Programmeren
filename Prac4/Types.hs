module Types where
import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree

data Tree1a = Leaf1a Number	| Node1a Number Tree1a Tree1a

pp1a :: Tree1a -> RoseTree
pp1a (Leaf1a number) = RoseNode (show number) []
pp1a (Node1a number child1 child2) = RoseNode (show number) [pp1a(child1), pp1a(child2)]

example1a = Node1a 12 
	(Node1a 4 
		(Leaf1a 3)
		(Leaf1a 6)
	) 
	(Leaf1a 2)

-- --------------------------------------------------------------------------------------------------------

data Tree1b = Leaf1b (Number,Number)	| Node1b (Number,Number) Tree1b Tree1b

pp1b :: Tree1b -> RoseTree
pp1b (Leaf1b tuple) = RoseNode (show tuple) []
pp1b (Node1b tuple child1 child2) = RoseNode (show tuple) [pp1b(child1), pp1b(child2)]

example1b = Node1b (12,24) (Leaf1b(1,3)) (Leaf1b (2,8))

-- --------------------------------------------------------------------------------------------------------

data Tree1c = Leaf1c 	| Node1c Number Tree1c Tree1c

pp1c :: Tree1c -> RoseTree
pp1c Leaf1c = RoseNode  "" []
pp1c (Node1c number child1 child2) = RoseNode (show number) [pp1c(child1), pp1c(child2)]

example1c = Node1c 12 
	(Node1c 10 
		(Node1c 4 
			(Node1c 3 Leaf1c Leaf1c)
			Leaf1c
		)
		Leaf1c
	) 
	(Node1c 18 Leaf1c Leaf1c)

example1c2 = Node1c 12 
	(Node1c 2 
		(Node1c 4 
			Leaf1c
			Leaf1c
		)
		Leaf1c
	) 
	(Node1c 10 Leaf1c Leaf1c)

-- --------------------------------------------------------------------------------------------------------

data Tree1d = Leaf1d (Number,Number)	| Node1d (Number,Number) Tree1d Tree1d

pp1d :: Tree1d -> RoseTree
pp1d (Leaf1d tuple) = RoseNode (show tuple) []
pp1d (Node1d tuple child1 child2) = RoseNode ("") [pp1d(child1), pp1d(child2)]

example1d = Node1d (12,24) 
	(Node1d (12,24) 
		(Leaf1d(1,3)) 
		(Leaf1d (2,8))) 
	(Node1d (12,24) 
		(Leaf1d(1,3)) 
		(Leaf1d (2,8)))
