
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


getBlockRow x sud y =  (fst (splitAt 3 (snd (splitAt (3*x ) (sud !! y)))))

getBlock :: Number -> Number -> Sudoku -> Block
getBlock x y sud = map (getBlockRow x sud) [(3*y),(3*y)+1, (3*y)+2]

-- The idea is to use one of the prac2 functions to rotate the matrix
getColumns :: Sudoku -> Sudoku
getColumns sud = error "unimplemented getColumns"
