module Solver where 
import Examples
import Prelude
import Data.List

type Sudoku = [[Square]]
type Square = [Int]
type Block = [[Square]] -- Same as sudoku, different meaning


{-
pass :: Sudoku -> Sudoku
pass sud = checkBlock $ checkColumn $ checkRow sud

checkBlock :: Sudoku -> Sudoku
checkBlock sud = error "unimplemented checkBlock"

checkColumn :: Sudoku -> Sudoku
checkColumn sud = checkRow $ getColumns sud

checkRow :: Sudoku -> Sudoku
checkRow (x:xs) = error "unimplemented checkRow"
-}
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
getInts :: [Square] -> [Int]
getInts ([x]:xs) = x : (getInts xs)
getInts (x:xs) = getInts xs
getInts [] = []

hsCheckRow :: [Square] -> [Square]
hsCheckRow row = map (mudiff  (getInts row)) row

hsCheck sud = map hsCheckRow (map hsCheckRow (getColumns sud))

test = hsCheckRow (head (prep exampleSudokuEasy))
test1 = map (getColumn exampleSudokuEasy) [0..8]
