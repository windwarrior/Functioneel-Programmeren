import FPPrac
import Prelude (Bool)
import Prelude (Int)

data Tree1a = Leaf1a Number	| Node1a Number Tree1a Tree1am

pp1a :: Tree1a -> RoseTree
pp1a tree = 