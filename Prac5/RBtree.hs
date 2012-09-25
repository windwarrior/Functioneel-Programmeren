data My_Color = Red | Black
	deriving (Show)
data My_RBTree = My_RBLeaf Color | My_RBNode Color Number RBTree RBTree
	deriving (Show)

myTree = My_RBNode Black 12 
	(My_RBLeaf Red)
	(My_RBLeaf Red)