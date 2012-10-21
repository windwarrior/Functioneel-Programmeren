import Prelude
import Sprockell
import Data.List

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
    N1 Op Expression 
    deriving (Eq,Show)
    
data Op =
    Plus |
    Min  |
    GreaterThen
    deriving (Eq,Show)
    
-- LookupTable is een lijst van Variabeleletters met adresnummers
type LookupTable = [(Char, Int)]
    
{-    
compile :: Statement -> [Assembly]
comp stat i j -- Stack Pointer

compileP :: Program -> [Asm]
-}
compileE :: Expression -> LookupTable -> ([Assembly], LookupTable)
compileE (Var c) lt 
    | freeAddr /= -1 = ([], lt ++ [(c, freeAddr)])
    | otherwise = error "Could not assign variable, main memory is full"
    where
        freeAddr = getFreeAdress lt

<<<<<<< HEAD
=======
compileE :: Expression -> [Assembly]

compileP :: Program -> [Assembly]-}
>>>>>>> 8fb20879a1f523c61ff6021173724312080b9b81

getFreeAdress :: LookupTable -> Int
getFreeAdress lt
    | length lt < dmemsize = ([0..(dmemsize-1)] \\ [(snd x) | x <- lt]) !! 0
    | otherwise = -1


vierkeervier = [
    (Assign (Var 'a') (Const 4)),
    (Assign (Var 'b') (Const 4)),
    (Assign (Var 'r') (Const 0)),
    (While (N2 GreaterThen (Var 'b') (Const 0)) [
        (Assign (Var 'r') (N2 Plus (Var 'r') (Var 'a'))),
        (Assign (Var 'b') (N2 Min  (Var 'b') (Const 1)))])]
        


