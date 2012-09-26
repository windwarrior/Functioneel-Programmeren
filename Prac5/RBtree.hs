module RBtree where
import FPPrac

data My_Color = Red | Black
	deriving (Show)
data My_RBTree = My_RBLeaf My_Color | My_RBNode My_Color Number My_RBTree My_RBTree
	deriving (Show)

myTree = My_RBNode Black 12 
	(My_RBLeaf Red)
	(My_RBNode Red 22 
		(My_RBLeaf Black)
		(My_RBLeaf Black)
	)
	

insert :: Number -> My_RBTree -> My_RBTree
insert n (My_RBNode color num child1 child2) 
    | n < num = (My_RBNode color num (insert n child1) child2) 
    | otherwise = (My_RBNode color num child1 (insert n child2))
insert n (My_RBLeaf color) = My_RBNode Red n (My_RBLeaf Red) (My_RBLeaf Red) 

rootToBlack :: My_RBTree -> My_RBTree
rootToBlack (My_RBLeaf color) = (My_RBLeaf Black) 
rootToBlack (My_RBNode color num child1 child2) = (My_RBNode Black num child1 child2) 

colourFlip :: My_RBTree -> My_RBTree
