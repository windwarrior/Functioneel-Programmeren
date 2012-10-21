import Prelude

data Statement = 
    Assign Expression Expression |    -- Hier moet op een of andere manier Var komen te staan, but ey
    If Expression [Statement] [Statement] |
    While Expression [Statement]
    deriving (Eq,Show)

type Program = [Statement]
    
data Expression =
    Var Char |
    Const Int |
    N2 Op Expression Expression |
    N1 Op Expression Expression 
    deriving (Eq,Show)
    
data Op =
    Plus |
    Min  |
    Gt
    deriving (Eq,Show)
    
{-    
compile :: Statement -> [Assembly]
comp stat i j -- Stack Pointer


compileE :: Expression -> [Assembly]

compileP :: Program -> [Assembly]-}



vierkeervier = [
    (Assign (Var 'a') (Const 4)),
    (Assign (Var 'b') (Const 4)),
    (Assign (Var 'r') (Const 0)),
    (While (N2 Gt (Var 'b') (Const 0)) [
        (Assign (Var 'r') (N2 Plus (Var 'r') (Var 'a'))),
        (Assign (Var 'b') (N2 Min  (Var 'b') (Const 1)))])]
        


