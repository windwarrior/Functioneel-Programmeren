pass :: Sudoku -> Sudoku
pass sud = checkBlock $ checkColumn $ checkRow sud

checkBlock :: Sudoku -> Sudoku
checkBlock sud = error "unimplemented"

checkColumn :: Sudoku -> Sudoku
checkColumn sud = error "unimplemented"

checkRow :: Sudoku -> Sudoku
checkRow sud = error "unimplemented"