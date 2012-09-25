module RBtree where
import FPPrac

data My_Color = Red | Black
	deriving (Show)
data My_RBTree = My_RBLeaf My_Color | My_RBNode My_Color Number My_RBTree My_RBTree
	deriving (Show)

myTree = My_RBNode Black 12 
	(My_RBLeaf Red)
	(My_RBLeaf Red)
