module Brandhout where
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
    
data Op = OpIncr | OpDecr | OpAdd | OpSub | OpMul | OpDiv | OpMod | OpEq | OpNEq | OpGt | OpLt | OpAnd | OpOr | OpNot | OpNoOp
    deriving (Eq,Show)
    
opToOpCode :: Op -> OpCode
opToOpCode OpIncr = Incr
opToOpCode OpDecr = Decr
opToOpCode OpAdd = Add
opToOpCode OpSub = Sub
opToOpCode OpMul = Mul
opToOpCode OpDiv = Div
opToOpCode OpMod = Mod
opToOpCode OpEq = Eq
opToOpCode OpNEq = NEq
opToOpCode OpGt = Gt
opToOpCode OpLt = Lt
opToOpCode OpAnd = And
opToOpCode OpOr = Or
opToOpCode OpNot = Not
opToOpCode OpNoOp = NoOp
    
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
        
compileE (Const c) store@CompileStore{stackPointer = sp} = ([Store (Imm c) sp], store{stackPointer = (sp + 1)})

compileE (N2 op exp1 exp2) store@CompileStore{stackPointer = sp} = (asm1 ++ asm2 ++ [(Load (Addr sp) 1), (Load (Addr (sp+1)) 2), (Calc (opToOpCode op) 1 2 1), (Store (Addr 1) sp)], store{stackPointer = (sp +1)})
    where
        (asm1, store1) = compileE exp1 store
        (asm2, store2) = compileE exp2 store1
        
--compileE (Const i) lt = ([Store Imm i (getFreeAdress

compileP :: (Program, CompileStore) -> [Assembly]
compileP ([], lt) = [EndProg]

{-
Een expressie zorgt er altijd voor dat er een waarde in register 1 komt te staan
-}
compileP (((Assign (Var e) exp2):xs), store@CompileStore{lookupTable = lt, stackPointer = sp}) = asm ++ [(Load (Addr sp) 1), (Store (Addr 1) addr)] ++ compileP (xs, newerStore)
    where
        (asm, newStore) = compileE exp2 store
        (addr, newerStore) = (getAddrOrFree e newStore)

compileP (((If exp stat1 stat2):xs), store)
    where
        (asm, newStore) = compileE exp store
        (asmif, ifStore) = compileP stat1 newStore
        (asmelse, elseStore) = compileP stat2 newStore
{-        
compileP lt ((If exp1 st1 st2):xs) = [] ++ compileP lt (xs) -- Lijst later in te vullen

compileP lt ((While exp1 st1):xs) = [] ++ compileP lt (xs) -- Lijst later in te vullen
-}
getAddrOrFree :: Char -> CompileStore -> (Int, CompileStore)
getAddrOrFree ch store@CompileStore{lookupTable = lt, stackBottom = sb}
    | ch `elem` [(fst x) | x <- lt] = (hitAddr, store)
    | length lt < dmemsize = (notAddr, store{lookupTable = (lt ++ [(ch, notAddr)])})
    | otherwise = (-1, store)
    
    where
        hitAddr = [i | (c,i) <- lt, c == ch] !! 0
        notAddr = ([1..(dmemsize-sb-1)] \\ [(snd x) | x <- lt]) !! 0
    

initStore :: CompileStore
initStore = CompileStore{lookupTable = [], stackBottom = 4, stackPointer = 4}   

vierkeervier = [
    (Assign (Var 'a') (Const 4)),
    (Assign (Var 'b') (Const 4)),
    (Assign (Var 'r') (Const 0)),
    (While (N2 OpGt (Var 'b') (Const 0)) [
        (Assign (Var 'r') (N2 OpAdd (Var 'r') (Var 'a'))),
        (Assign (Var 'b') (N2 OpSub  (Var 'b') (Const 1)))])]
        
{-
Assign (Var 'a') (Const 4)
-> Een adress verkrijgen (dus bijv 0x0)
-> 4 opslaan op dat adress
-> 'a' in de lookupTabletable zetten met dat adres
-}
