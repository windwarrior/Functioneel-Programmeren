import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

vervang :: Tree1a -> String -> Number ->  Tree1a
vervang (Leaf1a l) "" x = Leaf1a x
vervang (Node1a n ch1 ch2) "" x = Node1a x ch1 ch2
vervang (Node1a n ch1 ch2) (s:sx) x 
    | s == 'l' = Node1a n (vervang ch1 sx x) ch2
    | s == 'r' = Node1a n ch1 (vervamg ch2 sx x)

subboom :: Tree1a -> String -> Tree1a
subboom tree "" = tree
subboom (Leaf1a l) (s:sx) = error ("nope!")
subboom (Node1a n ch1 ch2) (s:sx)  
    | s == 'l' = subboom ch1 sx
    | s == 'r' = subboom ch2 sx
