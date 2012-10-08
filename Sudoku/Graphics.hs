import FPPrac
import FPPrac.Graphics
import Solver
import Prelude (Float)
main :: IO ()
main = graphicsout $ drawSudoku

sudokuSize = 400
halfSudokuSize = 0.5 * sudokuSize
blockSize = sudokuSize / 9

drawCircleAt :: Float -> Float -> Picture
drawCircleAt x y = Pictures[ translate x y $ Color black $ circleSolid 10]

drawLineFromTo :: (Float, Float) -> (Float, Float) -> Picture
drawLineFromTo  (x1, y1) (x2, y2) = Color black $ Line [(x1, y1), (x2, y2)]


drawSudoku :: Picture
drawSudoku = Pictures $
 (map (Line) [[(-halfSudokuSize + x * blockSize, halfSudokuSize), (-halfSudokuSize + x * blockSize, -halfSudokuSize)]| x <- [0..9]]) -- these are the vertical boardlines
 ++ (map (Line) [[(halfSudokuSize, -halfSudokuSize + x * blockSize), (-halfSudokuSize, -halfSudokuSize + x * blockSize)]| x <- [0..9]]) -- these are the horizontal boardlines
  ++ (drawNumbers exampleSudokuEasy):[]
  
drawNumbers :: Sudoku -> Picture
drawNumbers (x:xs) = drawLine x (0,0)

drawLine :: [Square] -> (Float, Float) ->  Picture
drawLine [] _ = Pictures $ []
drawLine (x:xs) (col, row) = [Translate (-halfSudokuSize + (col + 0.5) * blockSize) (-halfSudokuSize + (row + 0.5) * blockSize) $ Scale 0.15 0.15 $ Text x]:(drawLine xs (col+1, row))

row1 = [[ ],[ ],[ ],[8],[ ],[4],[9],[ ],[3]]
row2 = [[ ],[8],[1],[6],[3],[ ],[ ],[ ],[5]]
row3 = [[ ],[4],[7],[ ],[ ],[5],[ ],[ ],[ ]]
row4 = [[ ],[7],[6],[ ],[ ],[ ],[ ],[ ],[1]]
row5 = [[1],[ ],[9],[ ],[ ],[ ],[3],[ ],[6]]
row6 = [[8],[ ],[ ],[ ],[ ],[ ],[7],[5],[ ]]
row7 = [[ ],[ ],[ ],[4],[ ],[ ],[2],[8],[ ]]
row8 = [[4],[ ],[ ],[ ],[8],[2],[6],[1],[ ]]
row9 = [[6],[ ],[8],[9],[ ],[1],[ ],[ ],[ ]]
exampleSudokuEasy = [row1,row2,row3,row4,row5,row6,row7,row8,row9]