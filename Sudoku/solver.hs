module Solver where 
import Examples
import Prelude
import Data.List
import Debug.Trace

type Sudoku = [[Square]]
type Square = [Int]
type Block = [[Square]] -- Same as sudoku, different meaning

-------------------------------------------------------------------------------		
--                                        Getters                                           --
-------------------------------------------------------------------------------

getRow :: Int -> Sudoku -> [Square]
getRow y sud = sud !! y

getColumn :: Sudoku -> Int -> [Square]
getColumn sud x = map (!! x) sud

getColumns :: Sudoku -> Sudoku
getColumns sud = map (getColumn sud) [0..8]

getBlock :: Int -> Int -> Sudoku -> Block
getBlock x y sud = map (getBlockRow y sud) [(3*x)..(3*x)+2]
getBlockRow x sud y =  (take 3 (drop (3*x ) (sud !! y)))

getSquareAt :: (Int, Int) -> Sudoku -> Square
getSquareAt (x,y) sud = sqr
	where
		row = getRow y sud
		sqr = row !! x

-------------------------------------------------------------------------------		
--                                        Setters                                           --
-------------------------------------------------------------------------------

setSquare :: (Int, Int) -> Square -> Sudoku -> Sudoku
setSquare (x,y) sq sud = sudokuResult
	where
		row = getRow y sud
		(rowStart, rowEnd) = splitAt x row
		rowResult = rowStart ++ [sq] ++ (drop 1 rowEnd)
		(columnsStart, columnsEnd) = splitAt y sud
		sudokuResult = columnsStart ++ [rowResult] ++ (drop 1 columnsEnd)
		
-- GUI to solver functions
setSquareWithSafety :: (Int, Int) -> Int -> (Sudoku, Sudoku) -> (Sudoku, Bool)
setSquareWithSafety (x,y) num (sud_unsolved, sud_solved)
	| num `elem` (getSquareAt (x,y) sud_solved) = (setSquare (x,y) [num] sud_unsolved, True)
	| otherwise = (sud_unsolved, False)
	
setSmallSquare :: (Int, Int, Int) -> Sudoku -> Sudoku
setSmallSquare (x, y, num) sud
    | num `elem` square = setSquare (x,y) (square \\ [num]) sud
    | otherwise = setSquare (x,y) (square ++ [num]) sud
    where
        square = getSquareAt (x,y) sud


setBlock :: Block -> Int -> Int -> Sudoku -> Sudoku
setBlock block x y sud = firstRows ++ newMiddleRows ++ lastRows
	where
		firstRows = take (3*x) sud
		lastRows = drop (3*(x+1)) sud
		middleRows = take 3 (drop (3*x) sud)
		middleRowsBegin = map (take (3*y)) middleRows
		middleRowsEnd = map (drop (3*(y+1))) middleRows
		newMiddleRows = map (\x -> (middleRowsBegin !! x) ++ (block !! x) ++ (middleRowsEnd !! x)) [0..2]

-------------------------------------------------------------------------------		
--                                     Convertors                                         --
-------------------------------------------------------------------------------

blockToRow :: Block -> [Square]
blockToRow [] = []
blockToRow block = foldl1 (++) block

rowToBlock :: [Square] -> Block
rowToBlock row = [(fst rowBegin), (fst rowEnd), (snd rowEnd)]
	where
		rowBegin = splitAt 3 row
		rowEnd = splitAt 3 (snd rowBegin)

--Prepares sudoku for solver
prep :: Sudoku -> Sudoku
prep sud = map prepRow sud

prepRow :: [Square] -> [Square]
prepRow [] = []
prepRow (x:xs) 
	| x == [] = [1..9] : (prepRow xs)
	| otherwise = x : (prepRow xs)

-- Subtracts x from y, keeping single values
mudiff :: [Int] -> [Int] -> [Int]
mudiff x [y] = [y]
mudiff x y = (y \\ x)

-- Makes every combinations of pairs  where x<y for every pair [x,y]
-- Not used in implementation but comes in handy with harder algorithms
listComprehension :: [Int] -> [[Int]]
listComprehension [] =[]
listComprehension (x:xs) = (map (\xs -> [x,xs]) xs) ++ listComprehension xs

-- Returns list of single values
getNumbers :: [Square] -> [Int]
getNumbers ([x]:xs) = x : (getNumbers xs)
getNumbers (x:xs) = getNumbers xs
getNumbers [] = []

-------------------------------------------------------------------------------		
--                                     Checkers                                           --
-------------------------------------------------------------------------------

--Checks sudoku recursively till no more changes are made by f
recCheck :: (Sudoku -> Sudoku) -> Sudoku -> Sudoku
recCheck f sud
	| pass1 == pass2 = pass1
	| otherwise = recCheck f pass2
	where 
		pass1 = f sud
		pass2 = f pass1

--Checks whole sudoku (columns, rows and blocks respectively) once with f
checkSudoku :: ([Square] -> [Square]) -> Sudoku -> Sudoku
checkSudoku f sud = (checkBlocks f) (map f (getColumns (map f (getColumns sud))))

-- Checks every block with f
checkBlocks :: ([Square] -> [Square]) -> Sudoku -> Sudoku
checkBlocks f sud = foldl checkBlockColumn sud [0..2]
	where 
		checkBlockColumn = (\sud y->(foldl (\sud x -> (setBlock (rowToBlock (f (blockToRow (getBlock x y sud)))) x y sud)) sud [0..2]))

-- Solving algorithms, uses functions till result stays the same
solve :: Sudoku -> Sudoku
solve sud = solveWith [
	(recCheck vsCheck), 
	(recCheck hsCheck), 
	(recCheck npCheck), 
	(recCheck hpCheck)
	] 0 (prep sud)
	
solveWith :: [(Sudoku -> Sudoku)] -> Int -> Sudoku -> Sudoku
solveWith functions i sud 
	| i==n 					= sud --If hardest doesn't work return best effort
	| checked /= sud 	= solveWith functions 0 checked --Use first algorithm after solving with harder one
	| otherwise = solveWith functions (i+1) checked --If current algorithm doesn't work use harder one
	where 
		n = length functions
		checked = (functions !! i) sud
	
-------------------------------------------------------------------------------		
--                        Visibles Singles Algorithm                                 --
-------------------------------------------------------------------------------

vsCheck :: Sudoku -> Sudoku
vsCheck sud = checkSudoku (\row -> map (mudiff (getNumbers row)) row) sud

-------------------------------------------------------------------------------		
--                         Hidden Singles Algorithm                                 --
-------------------------------------------------------------------------------

hsCheck :: Sudoku -> Sudoku
hsCheck sud = checkSudoku removeHiddenSingles sud

getHiddenSingles :: [Square] -> [Square]
getHiddenSingles row = filter (\x -> length x == 1) (Data.List.group (Data.List.sort (foldl1 (++) row)))

removeHiddenSingles :: [Square] -> [Square]
removeHiddenSingles row 
	| singles == [] = row
	| otherwise = map (\sq -> filterHiddenSingles sq hs) row
	where
		singles = (getHiddenSingles row)
		hs = foldl1 (++) singles

filterHiddenSingles :: Square -> [Int] -> Square
filterHiddenSingles [x] _ = [x]
filterHiddenSingles sq hs
	| hasHiddenSingle = hiddenSingles
	| otherwise = sq
	where
		hiddenSingles = (intersect hs sq)
		hasHiddenSingle = not (hiddenSingles == [])

-------------------------------------------------------------------------------		
--                             Naked Pair Algorithm                                  --
-------------------------------------------------------------------------------

npCheck :: Sudoku -> Sudoku
npCheck sud = checkSudoku (\row -> removeNakedPairs row) sud

getNakedPairs :: [Square] -> [Square]
getNakedPairs row 
	| pairsOfpairs == [] = []
	| otherwise = nub (foldl1 (++) pairsOfpairs)
	where
		pairs = (Data.List.group (Data.List.sort (filter (\x -> length x == 2) row)))
		pairsOfpairs = (filter (\x -> length x == 2) pairs)

removeNakedPairs :: [Square] -> [Square]
removeNakedPairs row 
	| pairs == [] = row
	| otherwise = map (\x -> filterNakedPairs x pairs numbers) row
	where 
		pairs = getNakedPairs row
		numbers = nub (foldl1 (++) pairs)

filterNakedPairs :: Square -> [Square] -> [Int] -> Square
filterNakedPairs sq pairs numbers 
	| sq `elem` pairs = sq
	| otherwise = mudiff numbers sq

-------------------------------------------------------------------------------		
--                             Hidden Pair Algorithm                                 --
-------------------------------------------------------------------------------

hpCheck :: Sudoku -> Sudoku
hpCheck sud = checkSudoku (\row -> removeHiddenPairs row (filterHiddenPairs row)) sud

getHiddenPairs :: [Square] -> [Int]
getHiddenPairs row
	| grouped == [] = []
	| otherwise = duplicates
	where
		folded = foldl1 (++) row
		grouped = filter (\x -> length x == 2) (Data.List.group (Data.List.sort folded))
		duplicates = nub (foldl1 (++) grouped)

filterHiddenPairs :: [Square] -> ([Square], [Square])
filterHiddenPairs row
	| pairs /= [] = (intersection, result)
	| otherwise = ([], [])	
	where
		possiblePairs = getHiddenPairs row
		intersection = map (intersect possiblePairs) row
		pairs = (filter (\x -> length x ==2) (Data.List.group (Data.List.sort (filter (\x -> length x ==2) intersection))))
		result = nub $ foldl1 (++) pairs
		
--removeHiddenPairs row (intersection, pairs)
removeHiddenPairs :: [Square] -> ([Square],[Square]) -> [Square]
removeHiddenPairs (x:xs) (_,[]) = (x:xs)
removeHiddenPairs (x:xs) ([],_) = (x:xs)
removeHiddenPairs [] _ = []
removeHiddenPairs (x:xs) ((y:ys), pairs)
	| y `elem` pairs = y : (removeHiddenPairs xs (ys, pairs))
	| otherwise = x : (removeHiddenPairs xs (ys, pairs))

