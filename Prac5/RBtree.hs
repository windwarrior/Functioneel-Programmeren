
module RBtree where
import FPPrac

data My_Color = Red | Black | Grey
	deriving (Show)
data My_RBTree = My_RBLeaf My_Color | My_RBNode My_Color Number My_RBTree My_RBTree
	deriving (Show)
	
insert :: Number -> My_RBTree -> My_RBTree
insert n (My_RBNode color num child1 child2) 
	| n >= num = My_RBNode color num child1 (insert n child2)
	| otherwise = My_RBNode color num (insert n child1) child2
insert n (My_RBLeaf color) = My_RBNode Red n (My_RBLeaf Black) (My_RBLeaf Black)

rootToBlack :: My_RBTree -> My_RBTree
rootToBlack (My_RBNode Red n1 (My_RBLeaf Black) (My_RBNode Red n2 c1 c2)) = My_RBNode Black n1 (My_RBLeaf Red) (My_RBNode Red n2 c1 c2)
rootToBlack (My_RBNode Red n1 (My_RBNode Red n2 c1 c2) (My_RBLeaf Black)) = My_RBNode Black n1 (My_RBNode Red n2 c1 c2) (My_RBLeaf Red) 

colourFlip :: My_RBTree -> My_RBTree
-- colourFlip case 1
colourFlip (My_RBNode Black n1 
		(My_RBNode Red n2 
			(My_RBNode Red n3 c1 c2)
			(c3)
		)
		(My_RBNode Red n4 
			(c4)
			(c5)
		)
	) = (My_RBNode Red n1
		(My_RBNode Black n2
			(My_RBNode Red n3 c1 c2)
			(c3)
		)
		(My_RBNode Black n4
			(c4)
			(c5)
		)
	)
-- colourFlip case 2
colourFlip (My_RBNode Black n1 
		(My_RBNode Red n2 
			(c1)
			(My_RBNode Red n3 c2 c3)
		)
		(My_RBNode Red n4 
			(c4)
			(c5)
		)
	) = (My_RBNode Red n1
		(My_RBNode Black n2
			(c1)
			(My_RBNode Red n3 c2 c3)
		)
		(My_RBNode Black n4
			(c4)
			(c5)
		)
	)
-- colourFlip case 3
colourFlip (My_RBNode Black n1 
		(My_RBNode Red n2 
			(c1)
			(c2)
		)
		(My_RBNode Red n3 
			(My_RBNode Red n4 c3 c4)
			(c5)
		)
	) = (My_RBNode Red n1
		(My_RBNode Black n2
			(c1)
			(c2)
		)
		(My_RBNode Black n3
			(My_RBNode Red n4 c3 c4)
			(c5)
		)
	)
-- colourFlip case 3
colourFlip (My_RBNode Black n1 
		(My_RBNode Red n2 
			(c1)
			(c2)
		)
		(My_RBNode Red n3 
			(c4)
			(My_RBNode Red n4 c3 c5)
		)
	) = (My_RBNode Red n1
		(My_RBNode Black n2
			(c1)
			(c2)
		)
		(My_RBNode Black n3
			(c4)
			(My_RBNode Red n4 c3 c5)
		)
	)

rebalance :: My_RBTree -> My_RBTree
-- rebalance case 1
rebalance (My_RBNode Black n1 	--B
		(My_RBNode Red n2 				--R1
			(My_RBNode Red n3		 	--R2
				(My_RBLeaf _)
				(My_RBLeaf _)
			)
			(My_RBLeaf _)
		)
		(My_RBLeaf _)
	) = (My_RBNode Black n2 	--R1
		(My_RBNode Red n3  		--R2
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		(My_RBNode Red n1		--B
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	)
-- rebalance case 2
rebalance (My_RBNode Black n1 	--B
		(My_RBNode Red n2 				--R1
			(My_RBLeaf _)
			(My_RBNode Red n3		 	--R2
				(My_RBLeaf _)
				(My_RBLeaf _)
			)
		)
		(My_RBLeaf _)
	) = (My_RBNode Black n3 	--R2
		(My_RBNode Red n2  		--R1
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		(My_RBNode Red n1 		--B
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	)
-- rebalance case 3
rebalance (My_RBNode Black n1 	--B
		(My_RBLeaf _)
		(My_RBNode Red n2 				--R1
			(My_RBNode Red n3		 	--R2
				(My_RBLeaf _)
				(My_RBLeaf _)
			)
			(My_RBLeaf _)
		)
	) = (My_RBNode Black n3 	--R2
		(My_RBNode Red n1  		--B
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		(My_RBNode Red n2 		--R1
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	)
-- rebalance case 4
rebalance (My_RBNode Black n1 	--B
		(My_RBLeaf _)
		(My_RBNode Red n2 				--R1
			(My_RBLeaf _)
			(My_RBNode Red n3		 	--R2
				(My_RBLeaf _)
				(My_RBLeaf _)
			)
			
		)
	) = (My_RBNode Black n2 	--R1
		(My_RBNode Red n1  		--B
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		(My_RBNode Red n3 		--R2
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	)


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
