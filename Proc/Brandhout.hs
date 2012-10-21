import Prelude
import Sprockell
import Data.List
import Debug.Trace

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
{-compileE (Var c) lt 
    | freeAddr /= -1 = ([], lt ++ [(c, freeAddr)])
    | otherwise = error "Could not assign variable, main memory is full"
    where
        freeAddr = getFreeAdress lt
compileE (N2 op e1 e2) lt = 
     op (compileE e1 lt) (compileE e2 lt)
        
getFreeAdress :: LookupTable -> Int
getFreeAdress lt
    | length lt < dmemsize = ([0..(dmemsize-1)] \\ [(snd x) | x <- lt]) !! 0
    | otherwise = -1
        
compileE (Const c) lt = ([Load (Imm c) 1], lt)
        
--compileE (Const i) lt = ([Store Imm i (getFreeAdress

compileP :: (Program, LookupTable) -> [Assembly]
compileP ([], lt) = [EndProg]

{-
Een expressie zorgt er altijd voor dat er een waarde in register 1 komt te staan
-}
compileP (((Assign (Var e) exp2):xs), lt) = asm ++ [Store (Addr 1) addr] ++ compileP (xs, finalLt)
    where
        (asm, newLt) = compileE exp2 lt
        (addr, finalLt) = (getAddrOrFree e newLt)
{-        
compileP lt ((If exp1 st1 st2):xs) = [] ++ compileP lt (xs) -- Lijst later in te vullen

compileP lt ((While exp1 st1):xs) = [] ++ compileP lt (xs) -- Lijst later in te vullen
-}
getAddrOrFree :: Char -> LookupTable -> (Int, LookupTable)
getAddrOrFree ch lt
    | ch `elem` [(fst x) | x <- lt] = (hitAddr, lt)
    | length lt < dmemsize = (notAddr, lt ++ [(ch, notAddr)])
    | otherwise = (-1, lt)
    
    where
        hitAddr = [i | (c,i) <- lt, c == ch] !! 0
        notAddr = ([0..(dmemsize-1)] \\ [(snd x) | x <- lt]) !! 0

vierkeervier = [
    (Assign (Var 'a') (Const 4)),
    (Assign (Var 'b') (Const 4)),
    (Assign (Var 'r') (Const 0)),
    (While (N2 GreaterThen (Var 'b') (Const 0)) [
        (Assign (Var 'r') (N2 Plus (Var 'r') (Var 'a'))),
        (Assign (Var 'b') (N2 Min  (Var 'b') (Const 1)))])]
        
{-
Assign (Var 'a') (Const 4)
-> Een adress verkrijgen (dus bijv 0x0)
-> 4 opslaan op dat adress
-> 'a' in de lookuptable zetten met dat adres
-}

