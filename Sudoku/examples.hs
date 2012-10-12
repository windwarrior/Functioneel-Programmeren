module Examples where

import FPPrac

row1 = [[ ],[ ],[ ],	[8],[ ],[4],    [9],[ ],[3]]
row2 = [[ ],[8],[1],	[6],[3],[ ],    [ ],[ ],[5]]
row3 = [[ ],[4],[7],    [ ],[ ],[5],    [ ],[ ],[ ]]

row4 = [[ ],[7],[6],    [ ],[ ],[ ],    [ ],[ ],[1]]
row5 = [[1],[ ],[9],    [ ],[ ],[ ],    [3],[ ],[6]]
row6 = [[8],[ ],[ ],    [ ],[ ],[ ],    [7],[5],[ ]]

row7 = [[ ],[ ],[ ],    [4],[ ],[ ],    [2],[8],[ ]]
row8 = [[4],[ ],[ ],    [ ],[8],[2],    [6],[1],[ ]]
row9 = [[6],[ ],[8],    [9],[ ],[1],    [ ],[ ],[ ]]
exampleSudokuEasy = [row1,row2,row3,row4,row5,row6,row7,row8,row9]

exampleBlock =
	[	[
			[1,6,3,4],[2],[3]
		],	[
			[4],[5],[6]
		],	[
			[7],[8,3,6,1],[9]
	]	]