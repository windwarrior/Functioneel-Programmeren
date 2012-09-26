import FPPrac
import RBtree

insert :: Number -> My_RBTree -> My_RBTree
insert n (My_RBNode color num child1 child2) 
    | n < num = (My_RBNode color num (insert n child1) child2) 
    | otherwise = (My_RBNode color num child1 (insert n child2))
insert n (My_RBLeaf color) = My_RBNode Red n (My_RBLeaf Red) (My_RBLeaf Red) 
	
