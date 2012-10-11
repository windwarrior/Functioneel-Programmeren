module Solver where 
import Examples
import Prelude
import Data.List

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
getBlock x y sud = map (getBlockRow y sud) [(3*x),(3*x)+1, (3*x)+2]
getBlockRow x sud y =  (fst (splitAt 3 (snd (splitAt (3*x ) (sud !! y)))))

--SETTERS

setBlock :: Block -> Int -> Int -> Sudoku -> Sudoku
setBlock block x y sud = ((fst firstRows) ++ middleRows ++ (snd lastRows))
	where
		firstRows = splitAt (3*x) sud
		lastRows = splitAt 3 (snd firstRows)
		rowsBegin = map (\x -> (splitAt (3*y) ((fst lastRows) !! x))) [0..2]
		middleRows = map (\x -> ( fst (rowsBegin !! x)) ++ (block !! x) ++ (snd (splitAt 3 (snd (rowsBegin !! x))))) [0..2]

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

-- Returns list of single values
getNumbers :: [Square] -> [Int]
getNumbers ([x]:xs) = x : (getNumbers xs)
getNumbers (x:xs) = getNumbers xs
getNumbers [] = []

-- HIDDEN SINGLES ALGORITHM

hsCheck :: Sudoku -> Sudoku
hsCheck sud = checkSudoku (\row -> map (mudiff (getNumbers row)) row) sud

-- NAKED PAIR ALGORITHM

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

npCheck :: Sudoku -> Sudoku
npCheck sud = checkSudoku (\row -> removeNakedPairs row) sud

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
