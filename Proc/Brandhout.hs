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

compile :: Program -> [Assembly]
compile prog = compiled ++ [EndProg]
    where
        (compiled, store) = compileP prog initStore


compileE :: Expression -> CompileStore -> ([Assembly], CompileStore)
        
compileE (Const c) store@CompileStore{stackPointer = sp} = ([Store (Imm c) sp], store{stackPointer = (sp + 1)})

compileE (N2 op exp1 exp2) store@CompileStore{stackPointer = sp} = (asm1 ++ asm2 ++ [(Load (Addr sp) 1), (Load (Addr (sp+1)) 2), (Calc (opToOpCode op) 1 2 1), (Store (Addr 1) sp)], store{stackPointer = (sp +1)})
    where
        (asm1, store1) = compileE exp1 store
        (asm2, store2) = compileE exp2 store1
        
compileE (N1 op exp1 ) store@CompileStore{stackPointer = sp} = (asm1 ++ [(Load (Addr sp) 1), (Calc (opToOpCode op) 1 0 1), (Store (Addr 1) sp)], store{stackPointer = (sp)})
    where
        (asm1, store1) = compileE exp1 store
        
        
compileE (Var ch) store@CompileStore{stackPointer = sp} = ([(Load (Addr addr) 1), (Store (Addr 1) sp)], newStore{stackPointer = sp + 1})
    where
        (addr, newStore) = getAddrOrFree ch store
        

compileP :: Program -> CompileStore -> ([Assembly], CompileStore)
compileP [] st = ([], st)

compileP ((Assign (Var e) exp2):xs)  store@CompileStore{lookupTable = lt, stackPointer = sp} = (asm ++ [(Load (Addr sp) 1), (Store (Addr 1) addr)] ++ asmProg, finalStore)
    where
        (asm, newStore) = compileE exp2 store
        (addr, newerStore) = (getAddrOrFree e newStore)
        (asmProg, finalStore) = compileP xs newerStore

compileP ((While exp stats):xs) store = (asmExp ++ [(CJump 2), (Jump (lenProg + 1))] ++ asmStats ++ [(Jump (-(2 + lenProg + lenExp)))] ++ asmProg, finalStore)
    where
        (asmExp, newStore) = compileE exp store
        (asmStats, newerStore) = compileP stats newStore
        lenExp = length asmExp
        lenProg = length asmStats
        (asmProg, finalStore) = compileP xs newerStore

{-compileP (((If exp stat1 stat2):xs), store)
    where
        (asm, newStore) = compileE exp store
        (asmif, ifStore) = compileP stat1 newStore
        (asmelse, elseStore) = compileP stat2 newStore-}

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
        
