module RBtreeLB where
import FPPrac

data My_Color = Red | Black
	deriving (Show)
data My_RBTree = My_RBLeaf My_Color | My_RBNode My_Color Number My_RBTree My_RBTree
	deriving (Show)

myTree = My_RBNode Black 12 
	(My_RBLeaf Black)
	(My_RBNode Red 22 
		(My_RBLeaf Black)
		(My_RBLeaf Black)
	)
	
myTree2 = balancedInsert 7 $ balancedInsert 13 $ balancedInsert 3 $ balancedInsert 9 myTree

myTree3 = foldr bi (My_RBLeaf Black) [1..10]

insert :: Number -> My_RBTree -> My_RBTree
insert n (My_RBNode color num child1 child2) 
    | n < num = (My_RBNode color num (insert n child1) child2) 
    | otherwise = (My_RBNode color num child1 (insert n child2))
insert n (My_RBLeaf color) = My_RBNode Red n (My_RBLeaf Black) (My_RBLeaf Black) 

rootToBlack :: My_RBTree -> My_RBTree
rootToBlack (My_RBLeaf color) = (My_RBLeaf Black) 
rootToBlack (My_RBNode color num child1 child2) = (My_RBNode Black num child1 child2) 

{-
      N
     /\
    /  \
   A    B
  /
 / 
C
-}
colourFlip :: My_RBTree -> My_RBTree

--C zit aan node A child 1
colourFlip (My_RBNode Black n (My_RBNode Red a (My_RBNode Red c chc1 chc2) cha2) (My_RBNode Red b chb1 chb2)) = My_RBNode Red n (My_RBNode Black a (My_RBNode Red c chc1 chc2) cha2) (My_RBNode Black b chb1 chb2)

--C zit aan node A child 2
colourFlip (My_RBNode Black n (My_RBNode Red a cha1 (My_RBNode Red c chc1 chc2)) (My_RBNode Red b chb1 chb2)) = My_RBNode Red n (My_RBNode Black a cha1 (My_RBNode Red c chc1 chc2)) (My_RBNode Black b chb1 chb2)

--C zit aan node B child 1
colourFlip (My_RBNode Black n (My_RBNode Red a cha1 cha2) (My_RBNode Red b (My_RBNode Red c chc1 chc2) chb2)) = (My_RBNode Red n (My_RBNode Black a cha1 cha2) (My_RBNode Black b (My_RBNode Red c chc1 chc2) chb2))

--C zit aan node B child 2
colourFlip (My_RBNode Black n (My_RBNode Red a cha1 cha2) (My_RBNode Red b chb1 (My_RBNode Red c chc1 chc2))) = (My_RBNode Red n (My_RBNode Black a cha1 cha2) (My_RBNode Black b chb1 (My_RBNode Red c chc1 chc2)))

--Basecase, als het niet werkt, ga dan dit doen
colourFlip tree = tree

{-
      N
     /
    /  
   A  
  /
 /   
C
-}
rebalance :: My_RBTree -> My_RBTree

--C zit aan node A child 1
rebalance (My_RBNode Black a 
                (My_RBNode Red b
                    (My_RBNode Red c lll llr)
                    lr)
                r) = 
                    My_RBNode Black b 
                        (My_RBNode Red c
                            lll
                            llr)
                        (My_RBNode Red a 
                            lr
                            r )

--C zit aan node A child 2
rebalance (My_RBNode Black a 
                (My_RBNode Red b
					ll
					(My_RBNode Red c lrl lrr))
                r) =  
                    My_RBNode Black c 
                        (My_RBNode Red b
                            ll
                            lrl)
                        (My_RBNode Red a 
                            lrr
                            r )


--C zit aan node B child 2
rebalance (My_RBNode Black a
				l
                (My_RBNode Red b
					rl
					(My_RBNode Red c rrl rrr))
				)
                    =  
                    My_RBNode Black b 
                        (My_RBNode Red a
                            l
                            rl)
                        (My_RBNode Red c 
                            rrl
                            rrr )

rebalance (My_RBNode Black a
				l
                (My_RBNode Red b
					(My_RBNode Red c rll rlr)
					rr)
				)
                    =  
                    My_RBNode Black c 
                        (My_RBNode Red a
                            l
                            rll)
                        (My_RBNode Red b 
                            rlr
                            rr )
rebalance tree = tree

recRebalance :: My_RBTree -> My_RBTree
recRebalance (My_RBNode c a ch1 ch2) = rebalance (My_RBNode c a (recRebalance ch1) (recRebalance ch2))
recRebalance (My_RBLeaf c) = (My_RBLeaf c)

flipTree :: My_RBTree -> My_RBTree
flipTree (My_RBNode c a ch1 ch2) = colourFlip (My_RBNode c a (colourFlip ch1) (colourFlip ch2))
flipTree (My_RBLeaf c) = (My_RBLeaf c)

balancedInsert :: Number -> My_RBTree -> My_RBTree
balancedInsert n tree = recRebalance $ rootToBlack $ flipTree $ insert n tree

repair = rootToBlack . repair'
  where
    repair' (My_RBNode c a t1 t2) = (rebalance . colourFlip) (My_RBNode c a (repair' t1) (repair' t2))
    repair' t = t

bi n t = repair $ insert n t		

{-	
leftMostValue :: My_RBTree -> My_RBTree
leftmostValue My_RBNode _ number (My_RBLeaf _) _ = number
leftmostValue My_RBNode _ _ (My_RBNode _ child _) child = leftmostValue child

removeLeftmostNode ::  My_RBTree ->  My_RBTree
removeLeftmostNode (My_RBNode c1 n1 (My_RBNode c2 n2 ch1 ch2) ch3) = (My_RBNode c1 n1 (removeLeftmostNode (My_RBNode c2 n2 ch1 ch2)) ch3)
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBLeaf _) = My_RBLeaf Red
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBLeaf _) = My_RBLeaf Grey
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2) = (My_RBNode Red n2 c1 c2)
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2) = (My_RBNode Black n2 c1 c2)

			B							R1
		  /							  /     \
		R1		-> 		  R2		  B
	  /
	 R2
-}