module RBtree where

import FPPrac

data My_Color = Red | Black
data My_RBTree = My_RBLeaf My_Color | My_RBNode My_Color Number My_RBTree My_RBTree
