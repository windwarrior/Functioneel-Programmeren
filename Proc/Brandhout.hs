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
    N2 OpCode Expression Expression |
    N1 OpCode Expression 
    deriving (Eq,Show)

data CompileStore = CompileStore {
    lookupTable :: LookupTable,
    stackBottom :: Int,
    stackPointer :: Int
}
    
-- lookupTableTable is een lijst van Variabeleletters met adresnummers
type LookupTable = [(Char, Int)]

compile :: Program -> [Assembly]
compile prog = fixJump (compiled ++ [EndProg]) 0
    where
        (compiled, store) = compileP prog initStore


compileE :: Expression -> CompileStore -> ([Assembly], CompileStore)
        
compileE (Const c) store@CompileStore{stackPointer = sp, stackBottom = sb} = ([Store (Imm c) sp], store{stackPointer = (incStackPointer sb sp 1)})

compileE (N2 op exp1 exp2) store@CompileStore{stackPointer = sp, stackBottom = sb} = (asm1 ++ asm2 ++ [(Load (Addr sp) 1), (Load (Addr (sp+1)) 2), (Calc op 1 2 1), (Store (Addr 1) sp)], store{stackPointer = (incStackPointer sb sp 1)})
    where
        (asm1, store1) = compileE exp1 store --Deze store moet meegegeven naar de volgende, omdat de stackpointer verhoogd wordt
        (asm2, store2) = compileE exp2 store1
        
compileE (N1 op exp1 ) store@CompileStore{stackPointer = sp, stackBottom = sb} = (asm1 ++ [(Load (Addr sp) 1), (Calc op 1 0 1), (Store (Addr 1) sp)], store{stackPointer = (incStackPointer sb sp 1)})
    where
        (asm1, store1) = compileE exp1 store
        
        
compileE (Var ch) store@CompileStore{stackPointer = sp, stackBottom = sb} = ([(Load (Addr addr) 1), (Store (Addr 1) sp)], store{stackPointer = (incStackPointer sb sp 1)})
    where
        addr = getAddr ch store
        

compileP :: Program -> CompileStore -> ([Assembly], CompileStore)
compileP [] st = ([], st)

compileP ((Assign (Var e) exp2):xs)  store@CompileStore{lookupTable = lt, stackPointer = sp, stackBottom = sb} = (asm ++ [(Store (Addr 1) addr)] ++ asmProg, finalStore)
    where
        (asm, newStore) = compileE exp2 store
        (addr, newerStore) = (getAddrOrFree e newStore) -- Hier wordt de lookuptable aangepast, dus deze store moet meegegeven worden
        (asmProg, finalStore) = compileP xs newerStore{stackPointer = sb}

compileP ((While exp stats):xs) store@CompileStore{stackPointer = sp, stackBottom = sb} = (asmExp ++ [(CJump 2), (Jump (lenProg + 2))] ++ asmStats ++ [(Jump (-(2 + lenProg + lenExp)))] ++ asmProg, finalStore)
    where
        (asmExp, newStore) = compileE exp store
        (asmStats, newerStore) = compileP stats store{stackPointer = sb} -- het resultaat van bovenstaande compile actie is hoogstens de stackpointer aanpassen, maar die hebben we niet meer nodig
        lenExp = length asmExp
        lenProg = length asmStats
        (asmProg, finalStore) = compileP xs store{stackPointer = sb}

compileP ((If exp stat1 stat2):xs) store@CompileStore{stackPointer = sp, stackBottom = sb} = (asm ++ [(CJump (lenElse + 2))] ++ asmelse ++ [(Jump (lenIf + 1))] ++ asmif, newStore)
    where
        (asm, newStore) = compileE exp store -- the results of this parse step can be discarded, it it only possibly changing the state of the stackpointer, which is resetted after this step
        (asmif, ifStore) = compileP stat1 store{stackPointer = sb} -- if and else should not make adaptations to the state of the store, is this correct?
        (asmelse, elseStore) = compileP stat2 store{stackPointer = sb} -- interestingly, by compiling this way, we automatically implemented local scoping, without effort
        (asmProg, finalStore) = compileP xs store{stackPointer = sb}
        lenIf = length asmif
        lenElse = length asmelse
        
fixJump :: [Assembly] -> Int -> [Assembly]
fixJump [] _ = []
fixJump ((CJump i):xs) line = (CJump (line + i)) : fixJump xs (line+1)
fixJump ((Jump i):xs) line = (Jump (line +i)) : fixJump xs (line+1)
fixJump (x:xs) line = x : fixJump xs (line+1)



getAddrOrFree :: Char -> CompileStore -> (Int, CompileStore)
getAddrOrFree ch store@CompileStore{lookupTable = lt, stackBottom = sb}
    | ch `elem` [(fst x) | x <- lt] = (hitAddr, store)
    | length lt < dmemsize = (notAddr, store{lookupTable = (lt ++ [(ch, notAddr)])})
    | otherwise = (-1, store)
    
    where
        hitAddr = [i | (c,i) <- lt, c == ch] !! 0
        notAddr = ([1..(dmemsize-sb-1)] \\ [(snd x) | x <- lt]) !! 0
        
incStackPointer :: Int -> Int -> Int -> Int
incStackPointer min curr inc 
    | curr + inc < dmemsize && curr + inc >= min = curr + inc
    | otherwise = error ("Out of stack, requested " ++ (show (curr + inc)) ++ " where stack is between " ++ (show min) ++ " and " ++ (show dmemsize))
        
getAddr :: Char -> CompileStore -> Int
getAddr ch store@CompileStore{lookupTable = lt} 
    | ch `elem` [(fst x) | x <- lt] =  [i | (c,i) <- lt, c == ch] !! 0
    | otherwise = error ("Compile error, could not find initial assign of variable " ++ [ch])
    

initStore :: CompileStore
initStore = CompileStore{lookupTable = [], stackBottom = 4, stackPointer = 4}   
        
