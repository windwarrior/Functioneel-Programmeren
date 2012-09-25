module RBtree where

import FPPrac

data My_Color = Red | Black
data My_RBTree = My_RBLeaf Color | My_RBNode Color Number RBTree RBTree
