import FPPrac
import FPPrac.Graphics
import FPPrac.Events
import Prelude (Float, Int, read)
import Solver
import Debug.Trace
import Data.List ((\\))
import Data.Char
import Examples

-- Mooie plek voor een TODO:
	-- Kleine nummertjes in vakken kunnen wegstrepen
	-- Opslaan van files niet laten overschrijven
	-- Bij laden van files, error tonen als de file niet bestaat
	-- (x,y) -> (y,x)
	-- Niet altijd de hele sudoku hertekenen, nergens voor nodig
	-- Bij een lege sudoku klopt de validatie van velden niet :P

-------------------------------------------------------------------------------
--                                  Constanten                               --
-------------------------------------------------------------------------------

-- De grootte van het getekende bord
sudokuSize = 400
-- De grootte van een van de helften van het bord
halfSudokuSize = 0.5 * sudokuSize
-- De grootte van een veld van het bord
fieldSize = sudokuSize / 9
-- De groottte van een blok van 3*3
blockSize = sudokuSize / 3

empty = [[[] | x <- [0..8]] | y <- [0..8]]

-------------------------------------------------------------------------------
--                                    Store                                  --
-------------------------------------------------------------------------------

-- De state van het programma
data Store = Store {
    sudoku :: Sudoku,
	sudoku_solved :: Sudoku,
	numberPressed :: Int,
	wrongField :: (Int, Int),
	error_label :: String
}

-- Zet de initial state van de store
startStore :: Sudoku -> Store
startStore sud = Store {
    sudoku = sud,
	sudoku_solved = recCheck (vsCheck) $ prep sud,
	numberPressed = 0,
	wrongField = (-1,-1),
	error_label = ""
}

-------------------------------------------------------------------------------		
--                                    Input                                  --
-------------------------------------------------------------------------------

-- Functie om input te verwerken
processInput :: Store -> Input -> (Store,[Output])
processInput store@Store{sudoku = sudo, sudoku_solved = sudo_solv, numberPressed = num} (MouseDown (x,y)) 
	| f /= Nothing && num /= 0 && is_set = (store', o)
	| f /= Nothing && num /= 0 = (store_not_set, o_not_set)
	| otherwise    = (store, [])
	where
		(sudo_ins, is_set) = setSquareWithSafety i num (sudo, sudo_solv) 
		store' = store{sudoku = sudo_ins, sudoku_solved = recCheck (vsCheck) $ prep sudo_ins, numberPressed = 0, wrongField = (-1,-1), error_label=""}
		store_not_set = store{numberPressed = 0, wrongField = i, error_label=""}
		o = [ScreenClear, DrawPicture $ drawSudoku store']
		o_not_set = [ScreenClear, DrawPicture $ drawSudoku store_not_set]
		f = hitField (x,y)
		Just i = f

-- De functie die alle KeyIn events verwerkt
processInput store (KeyIn any)
	| any == 'h' = applyFunction (hsCheck) store
	| any == 'v' = applyFunction (vsCheck) store
	| any == 'n' = applyFunction (npCheck) store
	| any == 'p' = applyFunction (prep) store
	| any == 'o' = applyFunction (solve) store
	| any == 'r' = (store,[GraphPrompt ("Read sudoku", "filename")])
	| any == 's' = (store,[GraphPrompt ("save as", "filename")])
	| isNumber any = trace (show number) (store{numberPressed = number, wrongField = (-1,-1), error_label=""}, [ScreenClear, DrawPicture $ drawSudoku store{numberPressed = number, wrongField = (-1,-1), error_label=""}]) -- anders laat hij de selected niet zien :P
	| otherwise = (store, [])
	where
		number = read (any:"") :: Int

-- Het geval dat aangeroepen wordt op het moment dat het leesprompt klaar is
processInput store (Prompt ("Read sudoku", fileName))
	| fileName /= "" = (store, [ReadFile fileName (TXTFile "")])
	| otherwise = (store, [])

-- Het geval dat aangeroepen wordt als er een bestand ingeladen wordt	
processInput store (File fileName (TXTFile contents)) 
	| contents /= "" = (newStore, o)
	| otherwise = (store_error, o_error)
	where
		newSudoku = readSudoku contents
		newStore = Store{sudoku = newSudoku, sudoku_solved = recCheck (vsCheck) $ prep newSudoku, numberPressed = 0, wrongField = (-1,-1), error_label = ""}
		o = [ScreenClear, DrawPicture $ drawSudoku newStore]
		store_error = store{error_label="File is empty/does not exist"}
		o_error = [ScreenClear, DrawPicture $ drawSudoku store_error]

-- Het geval dat aangeroepen wordt als het saveprompt klaar is
processInput store@Store{sudoku = sudo} (Prompt ("save as", nm)) = (store, [SaveFile nm (TXTFile saveText)])
	where
		saveText = serializeSudoku sudo
		
-- Catch all other cases
processInput store _ = (store,[])

-- Functie om een locatie op het scherm om te zetten naar een vakje (x,y) op het bord
-- Misschien nog omzetten naar (y,x) omdat de solver daar ook mee werkt
-- Returned Nothing als er niets geraakt wordt, anders Just (x,y)
hitField :: (Float, Float) -> Maybe (Int, Int)
hitField (x,y) 
    | x >= -halfSudokuSize && x <= halfSudokuSize && y >= -halfSudokuSize && y <= halfSudokuSize = Just ((floor ((x+halfSudokuSize) / fieldSize)),(8 - floor ((y+halfSudokuSize) / fieldSize)))
    | otherwise = Nothing
	
-- Functie om een functie toe te passen op een sudoku, gebruikt om bepaalde strategieen te testen op een sudoku
applyFunction :: (Sudoku -> Sudoku) -> Store -> (Store, [Output])
applyFunction f store@Store{sudoku = sudo} = (store', o)
	where
		store' = store{sudoku = (f sudo), wrongField = (-1,-1)}
		o = [ScreenClear, DrawPicture $ drawSudoku store']

-------------------------------------------------------------------------------		
--                             Draw functions                                --
-------------------------------------------------------------------------------

-- Tekent de hele GUI
drawSudoku :: Store -> Picture
drawSudoku store@Store{sudoku = sudoku, numberPressed = num, wrongField = field, error_label = msg} = Pictures $
    (drawBackgrounds)
	++ (drawWrong field)
    ++ (map (Line) [[(-halfSudokuSize + x * fieldSize, halfSudokuSize), (-halfSudokuSize + x * fieldSize, -halfSudokuSize)]| x <- [0..9]]) -- these are the vertical boardlines
    ++ (map (Line) [[(halfSudokuSize, -halfSudokuSize + x * fieldSize), (-halfSudokuSize, -halfSudokuSize + x * fieldSize)]| x <- [0..9]]) -- these are the horizontal boardlines
    ++ (drawNumbers sudoku 0)
	++ drawStatusLine
	++ (drawSelectedNumber num)
	++ (drawError msg)

-- Tekent de grijze achtergronden om de 3*3 velden in een sudoku te markeren
-- roept drawSingleBackground aan met (x,y) locaties van een 3*3 blok
drawBackgrounds :: [Picture]
drawBackgrounds = (map drawSingleBackground [(-1,-1), (-1,1), (1,-1), (1,1), (0,0)])

-- Tekent een grijze achtergrond achter een enkel 3*3 vlak
drawSingleBackground :: (Float, Float) -> Picture
drawSingleBackground (xPos, yPos) = translate (xPos * blockSize) (yPos * blockSize) $ Color (greyN 0.9) $ rectangleSolid blockSize blockSize

-- Kleur een vakje waarin de gebruiker een verkeerd cijfer probeerde te zetten		
drawWrong :: (Int, Int) -> [Picture]
drawWrong (-1,-1) = []
drawWrong (x,y) = [Translate (-1 * halfSudokuSize + ((fromIntegral x) + 0.5) * fieldSize) (halfSudokuSize - ((fromIntegral y) + 0.5) * fieldSize) $ Color red $ rectangleSolid fieldSize fieldSize]

-- Tekent de nummers van een sudoku
drawNumbers :: Sudoku -> Float -> [Picture]
drawNumbers [] _ = []
drawNumbers (x:xs) row = (drawLine x (0,row)) ++ (drawNumbers xs (row+1))

-- Tekent een enkele regel van een sudoku
drawLine :: [Square] -> (Float, Float) ->  [Picture]
drawLine [] _ = []
drawLine (x:xs) (col, row)
    | FPPrac.length x == 1 = [Translate (-1 * halfSudokuSize + (col + 0.3) * fieldSize) (halfSudokuSize - (row + 0.7) * fieldSize) $ 
								Scale 0.2 0.2 $ 
									Color black $ 
										Text $ show (x !! 0)]  ++ (drawLine xs (col+1, row))
    | otherwise = [(Translate (-1 * halfSudokuSize + (col + 0.5) * fieldSize) (halfSudokuSize - (row + 0.5) * fieldSize) $ Pictures $ drawMultipleOption x (0, 0))] ++ (drawLine xs (col+1, row))

-- Tekent een vakje waarin meerdere mogelijkheden zijn
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

-- Tekent onderaan het scherm de mogelijke knoppen
drawStatusLine :: [Picture]
drawStatusLine = 
	[	Translate (-400) (-260) $ Scale 0.15 0.15 $ Text "Input:",
		Translate (-300) (-260) $ Scale 0.15 0.15 $ Text "[r]ead [s]ave",
		Translate (-400) (-290) $ Scale 0.15 0.15 $ Text "Solving:",
		Translate (-300) (-290) $ Scale 0.15 0.15 $ Text "s[o]lve [p]repare [n]akedPair [h]iddenSingle [v]isibleSingle"]

-- Tekent een string met welk cijfer geselecteerd is om te plaatsen
drawSelectedNumber :: Int -> [Picture]
drawSelectedNumber num
		| num /= 0 = [msg, msg_number]
		| otherwise = [msg, msg_no_number]
		where
			msg           = Translate (-400) (-230) $ Scale 0.15 0.15 $ Text "Selected:" 
			msg_number    = Translate (-300) (-230) $ Scale 0.15 0.15 $ Color red $ Text (show num)
			msg_no_number = Translate (-300) (-230) $ Scale 0.15 0.15 $ Color red $ Text "None"
			
drawError :: String -> [Picture]
drawError "" = []
drawError msg =
	[ Translate (-400) (280) $ Scale 0.15 0.15 $ Color red $ Text "Error: ",
	  Translate (-300) (280) $ Scale 0.15 0.15 $ Color red $ Text msg]
-------------------------------------------------------------------------------		
--                           Serializing sudokus                             --
-------------------------------------------------------------------------------

-- Functie om een sudoku te vertalen naar een string
serializeSudoku :: Sudoku -> String
serializeSudoku sudo = foldl (++) "" [serializeSudokuLine x | x <- sudo]

-- Functie om een regel van een sudoku om te zetten in een string
serializeSudokuLine :: [Square] -> String
serializeSudokuLine [] = "\n"
serializeSudokuLine (x:xs)
	| length x == 1 = (show (x!!0)) ++ serializeSudokuLine xs
	| otherwise = "." ++ serializeSudokuLine xs


-------------------------------------------------------------------------------		
--                          Deserializing sudokus                            --
-------------------------------------------------------------------------------

-- Functie om een sudoku te deserializen vanuit een file
readSudoku :: String -> Sudoku
readSudoku content = [readSudokuLine x | x <- (lines content)]

-- Functie om een enkele lijn vanuit een bestand om te zetten in een array van vakjes
readSudokuLine :: String -> [Square]
readSudokuLine [] = []
readSudokuLine (x:xs)
	| isNumber x = [[number]] ++ readSudokuLine xs
	| otherwise = [[]] ++ readSudokuLine xs
	where
		number = read (x:"") :: Int
		
-------------------------------------------------------------------------------		
--                             Main functions                                --
-------------------------------------------------------------------------------

-- De functie die de eventhandler installeerd zodat we ook input kunnen verwerken :)
main :: IO ()
main = installEventHandler "sudoku" processInput store startPic 10
    where 
        store = startStore empty
        startPic = drawSudoku store
		
