module TypesBinTree where
import FPPrac
import RoseTree

{- 
a: aan de knopen
b: aan de bladeren
 -}

data BinTree a b = BinNode a (BinTree a b) (BinTree a b) | BinLeaf b
    deriving Show
data Unit = U
    deriving Show

type BinTree1a = BinTree Number Number
type BinTree1b = BinTree (Number, Number) (Number, Number)
type BinTree1c = BinTree Number Unit

pp :: Show a => Show b => BinTree a b -> RoseTree
pp (BinLeaf val) = RoseNode (show val) []
pp (BinNode val node1 node2) = RoseNode (show val) [pp node1, pp node2]

