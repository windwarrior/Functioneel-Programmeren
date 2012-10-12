import FPPrac
import FPPrac.Graphics
import FPPrac.Events
import Prelude (Float, Int)
import Solver
import Debug.Trace
import Data.List ((\\))
import Examples

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
processKey store (KeyIn any)
	| any == 'h' = applyFunction (hsCheck) store
	| any == 'v' = applyFunction (vsCheck) store
	| any == 'n' = applyFunction (npCheck) store
	| otherwise = (store, [])
		
-- Catch all case
processKey store _ = (store,[])


applyFunction :: (Sudoku -> Sudoku) -> Store -> (Store, [Output])
applyFunction f store@Store{sudoku = sudo} = (store', o)
	where
		store' = Store{sudoku = (f sudo)}
		o = [ScreenClear, DrawPicture $ drawSudoku store']

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
  
drawNumbers :: Sudoku -> Float -> [Picture]
drawNumbers [] _ = []
drawNumbers (x:xs) row = (drawLine x (0,row)) ++ (drawNumbers xs (row+1))

drawLine :: [Square] -> (Float, Float) ->  [Picture]
drawLine [] _ = []
drawLine (x:xs) (col, row)
    | FPPrac.length x == 1 = [Translate (-1 * halfSudokuSize + (col + 0.3) * fieldSize) (halfSudokuSize - (row + 0.7) * fieldSize) $ 
								Scale 0.2 0.2 $ 
									Color black $ 
										Text $ show (x !! 0)]  ++ (drawLine xs (col+1, row))
    | otherwise = [(Translate (-1 * halfSudokuSize + (col + 0.5) * fieldSize) (halfSudokuSize - (row + 0.5) * fieldSize) $ Pictures $ drawMultipleOption x (0, 0))] ++ (drawLine xs (col+1, row))

drawMultipleOption :: Square -> (Int, Int) -> [Picture]
drawMultipleOption [] _ = []
drawMultipleOption g (x,y)
	| ((toIndex+1) `elem` g) = (Translate (fromIntegral (x - 1) * (fieldSize/3) - 0.25 * (fieldSize / 3)) (-1 * (fromIntegral (-1 + y) * (fieldSize/3) + 0.4 * (fieldSize / 3))) 
												$ Scale 0.1 0.1 
													$ Color (greyN 0.5)
														$ Text (show (toIndex + 1))):[] ++ (drawMultipleOption (g \\ [toIndex+1])(xnext, ynext))
	| otherwise = (drawMultipleOption (g) (xnext, ynext))
	where
		toIndex = (y*3 + x)
		xnext = (toIndex + 1) `mod` 3
		ynext = (toIndex + 1) `div` 3
		


doSudoku :: Sudoku ->  IO ()
doSudoku sud = installEventHandler "sudoku" processKey store startPic 10
    where 
        store = startStore sud
        startPic = drawSudoku store
   
