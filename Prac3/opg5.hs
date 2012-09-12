import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

zoek :: Number -> Tree1c -> Tree1c
zoek n Leaf1c = error ("not in tree")
zoek n (Node1c num child1 child2)
	| n == num 	= (Node1c num child1 child2)
	| n > num 	= zoek n child2
	| otherwise	= zoek n child1