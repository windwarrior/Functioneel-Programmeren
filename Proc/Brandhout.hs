import Prelude

data Statement = 
	Assign (Var Char) Expression |	
	If Expression [Statement] [Statement] |
	While Expression [Statement]
	
type Program = 
	[Statement]
	
data Expression =
	Var Char |
	Const Int |
	N2 Op Expression Expression |
	N1 Op Expression Expression 
    
data Op =
    Plus |
    Min
    
{-	
compile :: Statement -> [Asm]
comp stat i j -- Stack Pointer


compileE :: Expression -> [Asm]

compileP :: Program -> [Asm]-}



vierkeervier = [
    (Assign 'a' 4),
    (Assign 'b' 4),
    (Assign 'r' 0),
    (While ((Var 'b') > 0) [
        (Assign (Var 'r') ((Var 'r') Plus (Var 'a'))),
        (Assign (Var 'b') ((Var 'b') Min 1))])]
        


