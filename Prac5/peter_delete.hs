

leftmostValue :: My_RBTree -> Number
leftmostValue (My_RBNode _ number (My_RBLeaf _) _) = number
leftmostValue (My_RBNode _ _ (My_RBNode _ child _) child) = leftmostValue child

removeLeftmostNode ::  My_RBTree ->  My_RBTree
removeLeftmostNode (My_RBNode c1 n1 (My_RBNode c2 n2 ch1 ch2) ch3) = (My_RBNode c1 n1 (removeLeftmostNode (My_RBNode c2 n2 ch1 ch2)) ch3)
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBLeaf _) = My_RBLeaf Red
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBLeaf _) = My_RBLeaf Grey
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2) = (My_RBNode Red n2 c1 c2)
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2) = (My_RBNode Black n2 c1 c2)


greyColourFlip :: My_RBTree ->  My_RBTree
-- (a) all black
greyColourFlip (My_RBNode Black p 
		(My_RBLeaf Grey)
		(My_RBNode Black s
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	) = (My_RBNode Grey p
		(My_RBLeaf Black)
		(My_RBNode Red s
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	)
-- (b) l is red
greyColourFlip (My_RBNode c1 p 
		(My_RBLeaf Grey)
		(My_RBNode Black s
			(My_RBNode Red l 
				(My_RBLeaf Black)
				(My_RBLeaf Black)
			)
			r
		)
	) = (My_RBNode c1 l 
		(My_RBNode Black p
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		(My_RBNode Black s
				(My_RBLeaf Black)
				r
		)
	)
-- (c) p is red
greyColourFlip (My_RBNode Red p 
		(My_RBLeaf Grey)
		(My_RBNode Black s
			(My_RBLeaf Black)
			r
		)
	) = (My_RBNode Black s
		(My_RBNode Red p
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		r
	)
-- (d) r is red
greyColourFlip (My_RBNode Black p 
		(My_RBLeaf Grey)
		(My_RBNode Black s
			(My_RBLeaf Black)
			(My_RBLeaf Red)
		)
	) = (My_RBNode Black s
		(My_RBNode Black p
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
		(My_RBLeaf Black)
	)
-- (e) s is red
greyColourFlip (My_RBNode Black p 
		(My_RBLeaf Grey)
		(My_RBNode Red s
			(My_RBLeaf Black)
			(My_RBLeaf Black)
		)
	) = (My_RBNode Black s
		(My_RBNode Red p
			(My_RBLeaf Grey)
			(My_RBLeaf Black)
		)
		(My_RBLeaf Black)
	)