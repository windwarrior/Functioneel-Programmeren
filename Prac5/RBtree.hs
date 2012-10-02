
module RBtree where
import FPPrac
import Prelude (Bool)

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
myTree = My_RBNode Red 22 
	(My_RBNode Black 12 
		(My_RBNode Red 6 
			(My_RBNode Black 4
				(My_RBNode Red 3 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
				(My_RBNode Red 5 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
			)
			(My_RBNode Black 8
				(My_RBNode Red 7 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
				(My_RBNode Red 9 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
			)
		)
		(My_RBNode Red 17
			(My_RBNode Black 14 
				(My_RBNode Red 13 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
				(My_RBNode Red 15 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
			)
			(My_RBNode Black 19 
				(My_RBNode Red 18 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
				(My_RBNode Red 20 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)

			)
		)
	)
	(My_RBNode Black 32 
		(My_RBNode Red 27 
			(My_RBNode Black 25
				(My_RBNode Red 24 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
				(My_RBNode Red 26 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
			)
			(My_RBNode Black 29
				(My_RBNode Red 28 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
				(My_RBNode Red 30 
					(My_RBLeaf Black)
					(My_RBLeaf Black)
				)
			)
		)
		(My_RBNode Red 37
			(My_RBNode Black 36 
				(My_RBLeaf Red)
				(My_RBLeaf Red)
			)
			(My_RBNode Black 39 
				(My_RBLeaf Red)
				(My_RBLeaf Red)
			)
		)
	)
	
	
myInsertTree = insert 13 myTree

--DELETE

leftmostValue :: My_RBTree -> Number
leftmostValue (My_RBNode _ number (My_RBLeaf _) _) = number
leftmostValue (My_RBNode _ _ (My_RBNode c n child1 child2) _) = leftmostValue (My_RBNode c n child1 child2)

removeLeftmostNode ::  My_RBTree ->  My_RBTree
removeLeftmostNode (My_RBNode c1 n1 (My_RBNode c2 n2 ch1 ch2) ch3) = (My_RBNode c1 n1 (removeLeftmostNode (My_RBNode c2 n2 ch1 ch2)) ch3)
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBLeaf _)) = My_RBLeaf Red
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBLeaf _)) = My_RBLeaf Grey
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2)) = (My_RBNode Red n2 (My_RBLeaf Black) (My_RBLeaf Black))
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2)) = (My_RBNode Black n2 (My_RBLeaf Red) (My_RBLeaf Red))

(===) :: My_Color -> My_Color -> Bool
Grey === Grey = True
Red === Red = True
Black === Black = True
_ === _ = False


isGrey :: My_RBTree -> Bool
isGrey (My_RBLeaf c) = c === Grey
isGrey (My_RBNode c _ _ _) = c === Grey

isBlack :: My_RBTree -> Bool
isBlack (My_RBLeaf c) = c===Black
isBlack (My_RBNode c _ _ _) = c===Black

isRed :: My_RBTree -> Bool
isRed (My_RBLeaf c) = c===Red
isRed (My_RBNode c _ _ _) = c===Red

color :: My_Color -> My_RBTree -> My_RBTree
color c (My_RBLeaf _) = My_RBLeaf c
color c (My_RBNode _ n c1 c2) = My_RBNode c n c1 c2

isNode :: My_RBTree -> Bool
isNode (My_RBNode _ _ _ _) = True
isNode (My_RBLeaf _ ) = False

greyColourFlip :: My_RBTree ->  My_RBTree
-- (a) all black
greyColourFlip (My_RBNode Black p 
		g
		(My_RBNode Black s
			l
			r
		)
	) 
	| (isGrey g) && (isBlack l) && (isBlack r) 
	= (My_RBNode Grey p
			(color Black g)
			(My_RBNode Red s
				l
				r
			)
		)
-- (b) l is red
greyColourFlip (My_RBNode c1 p 
		g
		(My_RBNode Black s
			(My_RBNode Red l 
				a
				b
			)
			r
		)
	)
	| (isGrey g) && (isBlack a) && (isBlack b) 	
	= (My_RBNode c1 l 
			(My_RBNode Black p
				(color Black g)
				a
			)
			(My_RBNode Black s
					b
					r
			)
		)
-- (c) p is red
greyColourFlip (My_RBNode Red p 
		g
		(My_RBNode Black s
			l
			r
		)
	) 
	| (isGrey g) && (isBlack l)
	= (My_RBNode Black s
			(My_RBNode Red p
				(color Black g)
				l
			)
			r
		)
-- (d) r is red
greyColourFlip (My_RBNode Black p 
		g
		(My_RBNode Black s
			l
			r
		)
	)
	| (isGrey g)  && (isBlack l) && (isRed r)
	= (My_RBNode Black s
			(My_RBNode Black p
				(color Black g)
				l
			)
			(color Black r)
		)
-- (e) s is red
greyColourFlip (My_RBNode Black p 
		g
		(My_RBNode Red s
			l
			r
		)
	)
	| (isGrey g)  && (isBlack l) && (isBlack r)
	= (My_RBNode Black s
		(My_RBNode Red p
			g
			l
		)
		r
	)
-- mirrored
-- (a) all black
greyColourFlip (My_RBNode Black p 
		(My_RBNode Black s
			r
			l
		)
		g
	) 
	| (isGrey g) && (isBlack l) && (isBlack r) 
	= (My_RBNode Grey p
			(My_RBNode Red s
				r
				l
			)
			(color Black g)
		)
-- (b) l is red
greyColourFlip (My_RBNode c1 p 
		(My_RBNode Black s
			r
			(My_RBNode Red l 
				b
				a
			)
		)
		g
	)
	| (isGrey g) && (isBlack a) && (isBlack b) 	
	= (My_RBNode c1 l 
			(My_RBNode Black s
					r
					b
			)
			(My_RBNode Black p
				a
				(color Black g)
			)
		)
-- (c) p is red
greyColourFlip (My_RBNode Red p 
		(My_RBNode Black s
			r
			l
		)
		g
	) 
	| (isGrey g) && (isBlack l)
	= (My_RBNode Black s
			r
			(My_RBNode Red p
				l
				(color Black g)
			)
		)
-- (d) r is red
greyColourFlip (My_RBNode Black p 
		(My_RBNode Black s
			r
			l
		)
		g
	)
	| (isGrey g)  && (isBlack l) && (isRed r)
	= (My_RBNode Black s
			(color Black r)
			(My_RBNode Black p
				l
				(color Black g)
			)
		)
-- (e) s is red
greyColourFlip (My_RBNode Black p
		(My_RBNode Red s
			r
			l
		) 
		g
	)
	| (isGrey g)  && (isBlack l) && (isBlack r)
	= (My_RBNode Black s
		r
		(My_RBNode Red p
			l
			g
		)
	)
greyColourFlip other = other
	

greyRebalance :: My_RBTree -> My_RBTree
greyRebalance (My_RBLeaf c) = My_RBLeaf c
greyRebalance (My_RBNode c n c1 c2) = greyColourFlip (My_RBNode c n (greyRebalance c1) (greyRebalance c2))

delete :: Number -> My_RBTree -> My_RBTree
delete n (My_RBNode c num c1 c2)
	| (n == num) && (isNode c2) = My_RBNode c newValue c1 (removeLeftmostNode c2)--verwijderen 
	| (n == num) && not(isNode c2) = c1--verwijderen 
	| n < num = My_RBNode c num (delete n c1) c2 --zoek links
	| otherwise = My_RBNode c num c1 (delete n c2) --zoek rechts
	where newValue = leftmostValue c2

