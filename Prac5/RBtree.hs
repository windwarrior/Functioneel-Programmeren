module RBtree where
import FPPrac

data My_Color = Red | Black
	deriving (Show)
data My_RBTree = My_RBLeaf My_Color | My_RBNode My_Color Number My_RBTree My_RBTree
	deriving (Show)
	
insert :: Number -> My_RBTree -> My_RBTree
insert n (My_RBNode color num child1 child2) 
	| n >= num = My_RBNode color num child1 (insert n child2)
	| otherwise = My_RBNode color num (insert n child1) child2
insert n (My_RBLeaf color) = My_RBNode Red n (My_RBLeaf Red) (My_RBLeaf Red)

rootToBlack :: My_RBTree -> My_RBTree
rootToBlack (My_RBLeaf _) = My_RBLeaf Black
rootToBlack (My_RBNode color num c1 c2) = My_RBNode Black num c1 c2

colourFlip :: My_RBTree -> My_RBTree
colourFlip (My_RBLeaf Black) = My_RBLeaf Red
colourFlip (My_RBLeaf Red) = My_RBLeaf Black
colourFlip (My_RBNode Black num c1 c2) = My_RBNode Red num (colourFlip c1) (colourFlip c2)
colourFlip (My_RBNode Red num c1 c2) = My_RBNode Black num (colourFlip c1) (colourFlip c2)

-- examples for op3
baseTree2 = My_RBNode Black 1  	--N
	(My_RBLeaf Red) 						--A
	(My_RBNode Red 1 					--B
		(My_RBLeaf Red) 				
		(My_RBNode Red 1 				--C
			(My_RBLeaf Black) 
			(My_RBLeaf Black)
		)
	)
flippedBaseTree2 = colourFlip baseTree2
	
-- examples for op2
baseTree = My_RBNode Red 1 (My_RBLeaf Red) (My_RBLeaf Red)
blackBaseTree = rootToBlack baseTree

--- examples for op1
myTree = My_RBNode Black 12 
	(My_RBLeaf Red)
	(My_RBNode Red 22 
		(My_RBLeaf Black)
		(My_RBLeaf Black)
	)
	
myInsertTree = insert 13 myTree