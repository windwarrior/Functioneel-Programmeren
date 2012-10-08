module Solver where 
import FPPrac
import Examples 

type Sudoku = [[Square]]
type Square = [Number]
type Block = [[Square]] -- Same as sudoku, different meaning

pass :: Sudoku -> Sudoku
pass sud = checkBlock $ checkColumn $ checkRow sud

checkBlock :: Sudoku -> Sudoku
checkBlock sud = error "unimplemented checkBlock"

checkColumn :: Sudoku -> Sudoku
checkColumn sud = checkRow $ getColumns sud

checkRow :: Sudoku -> Sudoku
checkRow (x:xs) = error "unimplemented checkRow"

getBlock :: Number -> Number -> Sudoku -> Block
getBlock x y sud = error "unimplemented getBlock"

-- The idea is to use one of the prac2 functions to rotate the matrix
getColumns :: Sudoku -> Sudoku
getColumns sud = error "unimplemented getColumns"
