import FPPrac
import FPPrac.Graphics
import FPPrac.Events
import Prelude (Float, Int)
import Solver
import Debug.Trace
import Data.List ((\\))

-- De state van het programma
data Store = Store {
    sudoku :: Sudoku
}


-- De grootte van het getekende bord
sudokuSize = 400
-- De grootte van een van de helften van het bord
halfSudokuSize = 0.5 * sudokuSize
-- De grootte van een veld van het bord
fieldSize = sudokuSize / 9
-- De groottte van een blok van 3*3
blockSize = sudokuSize / 3

-- Zet de initial state van de store
startStore :: Sudoku -> Store
startStore sud = Store {
    sudoku = sud
}

-- Functie om input te verwerken
processKey :: Store -> Input -> (Store,[Output])
processKey store (MouseDown (x,y)) | f /= Nothing = trace (show i) (store, [])
                                   | otherwise    = (store, [])
	where
		f = hitField (x,y)
		Just i = f
-- Debug regel, print ingedrukte toets, matcht op KeyIn's only
processKey store (KeyIn a) = trace (show a) (store, [])
-- Catch all case
processKey store _ = (store,[])

-- Deze functie werkt, misschien even bepalen wat 0,0 is, bovenin of onderin :>
-- Er staat nu een mooie hack om het om te draaien
-- Stiekem is het wel een beetje hardcoded :x
hitField :: (Float, Float) -> Maybe (Int, Int)
hitField (x,y) 
    | x >= -halfSudokuSize && x <= halfSudokuSize && y >= -halfSudokuSize && y <= halfSudokuSize = Just ((floor ((x+halfSudokuSize) / fieldSize)),(8 - floor ((y+halfSudokuSize) / fieldSize)))
    | otherwise = Nothing

drawLineFromTo :: (Float, Float) -> (Float, Float) -> Picture
drawLineFromTo  (x1, y1) (x2, y2) = Color black $ Line [(x1, y1), (x2, y2)]

drawBackgrounds :: [Picture]
drawBackgrounds = (map drawSingleBackground [(-1,-1), (-1,1), (1,-1), (1,1), (0,0)])

drawSingleBackground :: (Float, Float) -> Picture
drawSingleBackground (xPos, yPos) = translate (xPos * blockSize) (yPos * blockSize) $ Color (greyN 0.9) $ rectangleSolid blockSize blockSize

drawSudoku :: Store -> Picture
drawSudoku store@Store{sudoku = sudoku} = Pictures $
    (drawBackgrounds)
    ++ (map (Line) [[(-halfSudokuSize + x * fieldSize, halfSudokuSize), (-halfSudokuSize + x * fieldSize, -halfSudokuSize)]| x <- [0..9]]) -- these are the vertical boardlines
    ++ (map (Line) [[(halfSudokuSize, -halfSudokuSize + x * fieldSize), (-halfSudokuSize, -halfSudokuSize + x * fieldSize)]| x <- [0..9]]) -- these are the horizontal boardlines
    ++ (drawNumbers sudoku 0)
	++ [Translate (-10) (-10) $ Scale 0.2 0.2 $ Text "A"]
  
drawNumbers :: Sudoku -> Float -> [Picture]
drawNumbers [] _ = []
drawNumbers (x:xs) row = (drawLine x (0,row)) ++ (drawNumbers xs (row+1))
--(Translate (-1 * halfSudokuSize + (col + 0.5) * fieldSize) (halfSudokuSize - (row + 0.5) * fieldSize) $
--Text $ show (x FPPrac.!! 0)):[]
drawLine :: [Square] -> (Float, Float) ->  [Picture]
drawLine [] _ = []
drawLine (x:xs) (col, row)
    | FPPrac.length x == 1 = [(Color green $ Text $ show (x !! 0)), (Color red $ rectangleSolid fieldSize fieldSize)]  ++ (drawLine xs (col+1, row))
    | otherwise = (drawMultipleOption x (0, 0)) ++ (drawLine xs (col+1, row))

drawMultipleOption :: Square -> (Int, Int) -> [Picture]
drawMultipleOption [] _ = []
drawMultipleOption g (x,y)
	| ((toIndex+1) `elem` g) = (Translate ((0.05 * (fieldSize/3)) - fromIntegral (x - 1) * (fieldSize/3)) ((0.05 * (fieldSize/3)) - (fromIntegral (-1 + y) * (fieldSize/3))) 
												$ Scale 0.1 0.1 
													$ Text (show (toIndex + 1))):[] ++ (drawMultipleOption (trace (show (g \\ [toIndex+1])) (g \\ [toIndex+1]))(xnext, ynext))
	| otherwise = (drawMultipleOption (g) (xnext, ynext))
	where
		toIndex = (y*3 + x)
		xnext = (toIndex + 1) `mod` 3
		ynext = (toIndex + 1) `div` 3
		
-- Dit moet nog even anders, maar het 'werkt'
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


doSudoku :: Sudoku ->  IO ()
doSudoku sud = installEventHandler "sudoku" processKey store startPic 10
    where 
        store = startStore sud
        startPic = drawSudoku store
   
