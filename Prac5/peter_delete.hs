

leftmostValue :: My_RBTree -> Number
leftmostValue (My_RBNode _ number (My_RBLeaf _) _) = number
leftmostValue (My_RBNode _ _ (My_RBNode _ child _) child) = leftmostValue child

removeLeftmostNode ::  My_RBTree ->  My_RBTree
removeLeftmostNode (My_RBNode c1 n1 (My_RBNode c2 n2 ch1 ch2) ch3) = (My_RBNode c1 n1 (removeLeftmostNode (My_RBNode c2 n2 ch1 ch2)) ch3)
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBLeaf _) = My_RBLeaf Red
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBLeaf _) = My_RBLeaf Grey
removeLeftmostNode (My_RBNode Red n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2) = (My_RBNode Red n2 c1 c2)
removeLeftmostNode (My_RBNode Black n1 (My_RBLeaf _) (My_RBNode _ n2 c1 c2) = (My_RBNode Black n2 c1 c2)

isGrey :: My_RBTree -> Bool
isGrey (My_RBLeaf c) = c==Grey
isGrey (My_RBNode c _ _) = c==Grey

isBlack :: My_RBTree -> Bool
isBlack (My_RBLeaf c) = c==Black
isBlack (My_RBNode c _ _) = c==Black

isRed :: My_RBTree -> Bool
isRed (My_RBLeaf c) = c==Red
isRed (My_RBNode c _ _) = c==Red

color :: Color -> My_RBTree -> My_RBTree
color c (My_RBLeaf _) = RBLeaf c
color c (My_RBNode _ n c1 c2) = My_RBnode c n c1 c2

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
	
greyRebalance :: My_RBTree -> My_RBTree
greyRebalance (My_RBLeaf c) = My_RBLeaf c
greyRebalance (My_RBNode c n c1 c2) = greyColourFlip (My_RBNode c n (greyColourFlip c1) (greyColourFlip c2))

