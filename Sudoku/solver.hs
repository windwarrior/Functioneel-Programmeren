module Solver where 
import FPPrac
import Examples
import Prelude (Int)
import Data.List

type Sudoku = [[Square]]
type Square = [Int]
type Block = [[Square]] -- Same as sudoku, different meaning

--GETTERS

getRow :: Number -> Sudoku -> [Square]
getRow y sud = sud FPPrac.!! y

getColumn :: Sudoku -> Number -> [Square]
getColumn sud x = map (FPPrac.!! x) sud

getColumns :: Sudoku -> Sudoku
getColumns sud = map (getColumn sud) [0..8]

getBlock :: Number -> Number -> Sudoku -> Block
getBlock x y sud = map (getBlockRow y sud) [(3*x),(3*x)+1, (3*x)+2]
getBlockRow x sud y =  (fst (FPPrac.splitAt 3 (snd (FPPrac.splitAt (3*x ) (sud FPPrac.!! y)))))

--setBlock :: Block -> Number -> Number -> Sudoku -> Sudoku
setBlock block x y sud = (fst firstRows, lastRows)
	where
		firstRows = FPPrac.splitAt (3*x) sud
		lastRows = FPPrac.splitAt 3 (snd firstRows)


--Prepares sudoku for solver

prep :: Sudoku -> Sudoku
prep sud = map prepRow sud

prepRow :: [Square] -> [Square]
prepRow [] = []
prepRow (x:xs) 
	| x == [] = [1..9] : (prepRow xs)
	| otherwise = x : (prepRow xs)

-- Subtracts x from y, keeping single values
mudiff :: [Number] -> [Number] -> [Number]
mudiff x [y] = [y]
mudiff x y = (y \\ x)

-- Returns list of single values
getNumbers :: [Square] -> [Number]
getNumbers ([x]:xs) = x : (getNumbers xs)
getNumbers (x:xs) = getNumbers xs
getNumbers [] = []

hsCheckRow :: [Square] -> [Square]
hsCheckRow row = map (mudiff  (getNumbers row)) row

hsCheck :: Sudoku -> Sudoku
hsCheck sud = map hsCheckRow (map hsCheckRow (getColumns sud))

test = hsCheckRow (head (prep exampleSudokuEasy))
test1 = map (getColumn exampleSudokuEasy) [0..8]
