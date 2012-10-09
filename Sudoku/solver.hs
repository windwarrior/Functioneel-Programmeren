module Solver where 
import FPPrac
import Examples 

type Sudoku = [[Square]]
type Square = [Number]
type Block = [[Square]] -- Same as sudoku, different meaning

-- STAAT EVEN UITGECOMMENT, WANT HET ERRORDE, DUBBELE IMPORTS
{-
pass :: Sudoku -> Sudoku
pass sud = checkBlock $ checkColumn $ checkRow sud

checkBlock :: Sudoku -> Sudoku
checkBlock sud = error "unimplemented checkBlock"

checkColumn :: Sudoku -> Sudoku
checkColumn sud = checkRow $ getColumns sud

checkRow :: Sudoku -> Sudoku
checkRow (x:xs) = error "unimplemented checkRow"

--GETTERS

getRow :: Number -> Sudoku -> [Square]
getRow y sud = sud !! y

getColumn :: Number -> Sudoku -> [Square]
getColumn x sud = map (!! x) sud

getBlock :: Number -> Number -> Sudoku -> Block
getBlock x y sud = map (getBlockRow x sud) [(3*y),(3*y)+1, (3*y)+2]
getBlockRow x sud y =  (fst (splitAt 3 (snd (splitAt (3*x ) (sud FPPrac.!! y)))))

-}