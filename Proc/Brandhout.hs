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


data CompileStore = CompileStore {
    lookupTable :: LookupTable,
    stackBottom :: Int,
    stackPointer :: Int
}
    
-- lookupTableTable is een lijst van Variabeleletters met adresnummers
type LookupTable = [(Char, Int)]


{-    
compile :: Statement -> [Assembly]
comp stat i j -- Stack Pointer

compileP :: Program -> [Asm]
-}
compileE :: Expression -> CompileStore -> ([Assembly], CompileStore)
{-compileE (Var c) lt 
    | freeAddr /= -1 = ([], lt ++ [(c, freeAddr)])
    | otherwise = error "Could not assign variable, main memory is full"
    where
        freeAddr = getAddrOrFree c lt-}
        
compileE (Const c) store = ([Load (Imm c) 1], store)
        
--compileE (Const i) lt = ([Store Imm i (getFreeAdress

compileP :: (Program, CompileStore) -> [Assembly]
compileP ([], lt) = [EndProg]

{-
Een expressie zorgt er altijd voor dat er een waarde in register 1 komt te staan
-}
compileP (((Assign (Var e) exp2):xs), store@CompileStore{lookupTable = lt}) = asm ++ [Store (Addr 1) addr] ++ compileP (xs, newerStore)
    where
        (asm, newStore) = compileE exp2 store
        (addr, newerStore) = (getAddrOrFree e newStore)
{-        
compileP lt ((If exp1 st1 st2):xs) = [] ++ compileP lt (xs) -- Lijst later in te vullen

compileP lt ((While exp1 st1):xs) = [] ++ compileP lt (xs) -- Lijst later in te vullen
-}
getAddrOrFree :: Char -> CompileStore -> (Int, CompileStore)
getAddrOrFree ch store@CompileStore{lookupTable = lt}
    | ch `elem` [(fst x) | x <- lt] = (hitAddr, store)
    | length lt < dmemsize = (notAddr, store{lookupTable = (lt ++ [(ch, notAddr)])})
    | otherwise = (-1, store)
    
    where
        hitAddr = [i | (c,i) <- lt, c == ch] !! 0
        notAddr = ([0..(dmemsize-1)] \\ [(snd x) | x <- lt]) !! 0
    

initStore :: CompileStore
initStore = CompileStore{lookupTable = [], stackBottom = 4, stackPointer = 4}   

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
-> 'a' in de lookupTabletable zetten met dat adres
-}

