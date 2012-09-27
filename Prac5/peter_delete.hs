

leftmostValue :: My_RBTree -> Number
leftmostValue (My_RBNode _ number (My_RBLeaf _) _) = number
leftmostValue (My_RBNode _ _ (My_RBNode _ child _) child) = leftmostValue child

removeLeftmostNode ::  My_RBTree ->  My_RBTree
removeLeftmostNode (My_RBNode c1 n1 (My_RBNode c2 n2 ch1 ch2) ch3) = (My_RBNode c1 n1 (removeLeftmostNode (My_RBNode c2 n2 ch1 ch2)) ch3)
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf Black) (My_RBLeaf Black) = My_RBLeaf Red
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf Red) (My_RBLeaf Red) = My_RBLeaf Grey
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf Black) (My_RBNode Black n2 c1 c2) = (My_RBNode Red n2 c1 c2)
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf Red) (My_RBNode Red n2 c1 c2) = (My_RBNode Black n2 c1 c2)