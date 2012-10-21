import Prelude

data Statement = 
	Assign Expression Expression |	-- Hier moet op een of andere manier Var komen te staan, but ey
	If Expression [Statement] [Statement] |
	While Expression [Statement]

type Program = [Statement]
	
data Expression =
	Var Char |
	Const Int |
	N2 Op Expression Expression |
	N1 Op Expression Expression 
    
data Op =
    Plus |
    Min  |
    Gt
    
{-	
compile :: Statement -> [Asm]
comp stat i j -- Stack Pointer


compileE :: Expression -> [Asm]

compileP :: Program -> [Asm]-}



vierkeervier = [
    (Assign (Var 'a') (Const 4)),
    (Assign (Var 'b') (Const 4)),
    (Assign (Var 'r') (Const 0)),
    (While (N2 Gt (Var 'b') (Const 0)) [
        (Assign (Var 'r') (N2 Plus (Var 'r') (Var 'a'))),
        (Assign (Var 'b') (N2 Min  (Var 'b') (Const 1)))])]
        


