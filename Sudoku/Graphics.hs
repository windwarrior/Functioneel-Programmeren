import FPPrac
import FPPrac.Graphics
import FPPrac.Events
import Solver
import Debug.Trace

data Store = Store {
    sudoku :: Sudoku
}

sudokuSize = 400
halfSudokuSize = 0.5 * sudokuSize
fieldSize = sudokuSize / 9
blockSize = sudokuSize / 3

main :: IO ()
main = graphicsout $ drawSudoku

startStore :: Store
startStore = Store {
    sudoku = exampleSudokuEasy
    }

processKey :: Store -> Input -> (Store,[Output])
processKey store (KeyIn a) = trace (show a) (store, [])
processKey store _ = (store,[])

drawCircleAt :: Float -> Float -> Picture
drawCircleAt x y = Pictures[ translate x y $ Color black $ circleSolid 10]

drawLineFromTo :: (Float, Float) -> (Float, Float) -> Picture
drawLineFromTo  (x1, y1) (x2, y2) = Color black $ Line [(x1, y1), (x2, y2)]

drawBackgrounds :: [Picture]
drawBackgrounds = (map drawSingleBackground [(-1,-1), (-1,1), (1,-1), (1,1), (0,0)])

drawSingleBackground :: (Float, Float) -> Picture
drawSingleBackground (xPos, yPos) = translate (xPos * blockSize) (yPos * blockSize) $ Color (greyN 0.9) $ rectangleSolid blockSize blockSize

drawSudoku :: Picture
drawSudoku = Pictures $
    (drawBackgrounds)
    ++ (map (Line) [[(-halfSudokuSize + x * fieldSize, halfSudokuSize), (-halfSudokuSize + x * fieldSize, -halfSudokuSize)]| x <- [0..9]]) -- these are the vertical boardlines
    ++ (map (Line) [[(halfSudokuSize, -halfSudokuSize + x * fieldSize), (-halfSudokuSize, -halfSudokuSize + x * fieldSize)]| x <- [0..9]]) -- these are the horizontal boardlines
    ++ (drawNumbers exampleSudokuEasy 0)
  
drawNumbers :: Sudoku -> Float -> [Picture]
drawNumbers [] _ = []
drawNumbers (x:xs) row = (drawLine x (0,row)) ++ (drawNumbers xs (row+1))

drawLine :: [Square] -> (Float, Float) ->  [Picture]
drawLine [] _ = []
drawLine (x:xs) (col, row)
    | FPPrac.length x == 1 = (Translate (-1 * halfSudokuSize + (col + 0.5) * fieldSize) (halfSudokuSize - (row + 0.5) * fieldSize) $ Scale 0.1 0.1 $ Text $ show (x FPPrac.!! 0)):[]  ++ (drawLine xs (col+1, row))
    | otherwise = (drawLine xs (col+1, row))

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


doSudoku :: IO ()
doSudoku = installEventHandler "sudoku" processKey startStore startPic 10
    where 
        startPic = drawSudoku