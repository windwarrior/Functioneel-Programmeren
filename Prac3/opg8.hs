import FPPrac
import Prelude (Bool)
import Prelude (Int)
import RoseTree
import Opg1

testBalanced :: Tree1c -> Boolean
testBalanced tree = maxDepth tree - minDepth tree < 2


minDepth :: Tree1c -> Number
minDepth (Leaf1c) = 1
minDepth (Node1c n ch1 ch2) = 1 + minimun [(minDepth ch1),(mindept ch2)]

maxDept :: Tree1c -> Number
maxDept (Leaf1c) = 1
maxDept (Node1c n ch1 ch2) = 1 + maximum [(minDepth ch1),(mindept ch2)]


--

makeBalanced :: Tree1c -> Tree1c
makeBalanced tree = makeBalancedList (makeList tree)

makeBalancedList :: [Number] -> Tree1c
makeBalancedList [] = Leaf1c
makeBalancedList (x:xs)
    | xs == [] = Node1c x Leaf1c Leaf1c
    | otherwise = Node1c x (makeBalancedList $ take (lg / 2) xs) (makeBalancedList $ drop (lg / 2) xs)
    where
        lg = length xs
        
