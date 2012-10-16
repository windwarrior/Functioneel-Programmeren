module Solver where 
import Examples
import Prelude
import Data.List
import Debug.Trace

type Sudoku = [[Square]]
type Square = [Int]
type Block = [[Square]] -- Same as sudoku, different meaning

--GETTERS

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

--SETTERS
{-
setBlock :: Block -> Int -> Int -> Sudoku -> Sudoku
setBlock block x y sud = ((fst firstRows) ++ middleRows ++ (snd lastRows))
	where
		firstRows = splitAt (3*x) sud
		lastRows = splitAt 3 (snd firstRows)
		rowsBegin = map (\x -> (splitAt (3*y) ((fst lastRows) !! x))) [0..2]
		middleRows = map (\x -> ( fst (rowsBegin !! x)) ++ (block !! x) ++ (snd (splitAt 3 (snd (rowsBegin !! x))))) [0..2]
-}		
setSquare :: (Int, Int) -> Int -> Sudoku -> Sudoku
setSquare (x,y) num sud = sudokuResult
	where
		row = getRow y sud
		(rowStart, rowEnd) = splitAt x row
		rowResult = rowStart ++ [[num]] ++ (drop 1 rowEnd)
		(columnsStart, columnsEnd) = splitAt y sud
		sudokuResult = columnsStart ++ [rowResult] ++ (drop 1 columnsEnd)
-- GUI to solver functions

setSquareWithSafety :: (Int, Int) -> Int -> (Sudoku, Sudoku) -> (Sudoku, Bool)
setSquareWithSafety (x,y) num (sud_unsolved, sud_solved)
	| num `elem` (getSquareAt (x,y) sud_solved) = (setSquare (x,y) num sud_unsolved, True)
	| otherwise = (sud_unsolved, False)

setBlock block x y sud = firstRows ++ newMiddleRows ++ lastRows
	where
		firstRows = take (3*x) sud
		lastRows = drop (3*(x+1)) sud
		middleRows = take 3 (drop (3*x) sud)
		middleRowsBegin = map (take (3*y)) middleRows
		middleRowsEnd = map (drop (3*(y+1))) middleRows
		newMiddleRows = map (\x -> (middleRowsBegin !! x) ++ (block !! x) ++ (middleRowsEnd !! x)) [0..2]

-- CONVERTORS

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


listComprehension :: [Int] -> [[Int]]
listComprehension [] =[]
listComprehension (x:xs) = (map (\xs -> [x,xs]) xs) ++ listComprehension xs

-- Returns list of single values
getNumbers :: [Square] -> [Int]
getNumbers ([x]:xs) = x : (getNumbers xs)
getNumbers (x:xs) = getNumbers xs
getNumbers [] = []

-- CHECKERS

recCheck :: (Sudoku -> Sudoku) -> Sudoku -> Sudoku
recCheck f sud
	| pass1 == pass2 = pass1
	| otherwise = recCheck f pass2
	where 
		pass1 = f sud
		pass2 = f pass1

checkSudoku :: ([Square] -> [Square]) -> Sudoku -> Sudoku
checkSudoku f sud = (checkBlocks f) (map f (getColumns (map f (getColumns sud))))

checkBlocks :: ([Square] -> [Square]) -> Sudoku -> Sudoku
checkBlocks f sud = foldl checkBlockColumn sud [0..2]
	where 
		checkBlockColumn = (\sud y->(foldl (\sud x -> (setBlock (rowToBlock (f (blockToRow (getBlock x y sud)))) x y sud)) (sud) [0..2]))

--TODO lijst van functies
solve :: Sudoku -> Sudoku
solve sud
	| second /= first = solve second
	| third /= second = solve third
	| fourth /= third = solve fourth
	| otherwise = fourth
	where 
		first = recCheck vsCheck sud
		second = recCheck hsCheck first
		third = recCheck npCheck second
		fourth = recCheck hpCheck third

-- VISIBLE SINGLES ALGORITHM

vsCheck :: Sudoku -> Sudoku
vsCheck sud = checkSudoku (\row -> map (mudiff (getNumbers row)) row) sud

-- HIDDEN SINGLES ALGORITHM

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
filterHiddenSingles sq hs
	| hasHiddenSingle = hiddenSingles
	| otherwise = sq
	where
		hiddenSingles = (intersect hs sq)
		hasHiddenSingle = not (hiddenSingles == [])

-- NAKED PAIR ALGORITHM

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

-- HIDDEN PAIR ALGORITHM

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

