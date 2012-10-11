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

setBlock :: Block -> Int -> Int -> Sudoku -> Sudoku
setBlock block x y sud = ((fst firstRows) ++ middleRows ++ (snd lastRows))
	where
		firstRows = splitAt (3*x) sud
		lastRows = splitAt 3 (snd firstRows)
		rowsBegin = map (\x -> (splitAt (3*y) ((fst lastRows) !! x))) [0..2]
		middleRows = map (\x -> ( fst (rowsBegin !! x)) ++ (block !! x) ++ (snd (splitAt 3 (snd (rowsBegin !! x))))) [0..2]

blockToRow :: Block -> [Square]
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

hsCheckRow :: [Square] -> [Square]
hsCheckRow row = map (mudiff  (getNumbers row)) row

hsCheckBlock :: Block -> Block
hsCheckBlock block = rowToBlock (hsCheckRow (blockToRow block))

hsCheckBlocks :: Sudoku -> Sudoku
hsCheckBlocks sud = foldl hsCheckBlockColumn sud [0..2]
	where 
		hsCheckBlockColumn = (\sud y->(foldl (\sud x -> (setBlock (hsCheckBlock (getBlock x y sud)) x y sud)) (sud) [0..2]))

hsCheck :: Sudoku -> Sudoku
hsCheck sud = hsCheckBlocks (map hsCheckRow (getColumns (map hsCheckRow (getColumns sud))))

test = hsCheckRow (head (prep exampleSudokuEasy))
test1 = map (getColumn exampleSudokuEasy) [0..8]
