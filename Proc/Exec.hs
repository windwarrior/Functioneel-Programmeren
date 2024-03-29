{-# LANGUAGE RecordWildCards #-}

module Exec where

import Sprockell
import Brandhout
import Prelude

import Debug.Trace

data Tick = Tick
	deriving (Eq,Show)

clock = Tick : clock


exec prog state (i:is)
	| prog!!(pc state)==EndProg	= []
	| otherwise			= output prog state' : exec prog state' is

		where
		  state' = sprockell prog state i


test prog = output prog initstate
          : exec prog initstate clock


main = putStr . unlines . map show $ test programma


{-----------------------------------
| To simulate: execute main
|
| Below:
| - choose the program that you want to simulate (testprog),
| - define your own output function.
-----------------------------------}

-- Hier de verwijzing aanpassen
programma = vierkeerviercomp

-- Hier extra programmas definieren
localcompile = compile ifElseLocalScope
vierkeerviercomp = compile vierkeervier
complexmathcom = compile complexMath
ifelsecom = compile ifelseexample
simpleexpressioncomp = compile simpleexpression
testprog = compile [Assign (Var 'a') ((N2 Add (N2 Add (Const 4) (Const 7)) (Const 4)))]
testprog1 = compile [Assign (Var 'a') ((N1 Not (Const 0)))]
testwhile = compile [
        Assign (Var 'a') (Const 1),
        While (N2 Eq (Var 'a') (Const 1)) [Assign (Var 'a') (Const 0)],
        Assign (Var 'a') (Const 1)
    ]

output prog (state@State{..}) = (dmem, regbank, pc, prog!!pc) -- , regbank!!2, regbank!!3)
						-- Note: field names from the state
						-- 	 are usable as variables

printMore :: [Assembly] -> Int -> Char
printMore [] i = ' '
printMore (x:xs) i = trace ((show i) ++ " " ++ (show x)) (printMore xs (i+1))


{-----------------------------------
| Example 1
------------------------------------

Calculate 3^5 (= 243)

Program in pseudo-code:

a = 3;
n = 5;
power = 1;
while (n != 0) {
  power = a * power;
  n = n-1;
};

Below: prog1 is assembly code for the Sprockell
that will be executed when main is evaluated.

The first three instructions put variables a,n,power in registers 2,3,4 (respectively).

NOTE: this is done BY HAND, to keep the program manageable. A compiler
should NOT bind a variable to a register, but to an address in main memory (dmem).
Also the assignment has to be done to these addresses in dmem, and NOT to registers,
as in the code below.
-----------------------------------}


prog1 = [ Load (Imm 3) 2	-- 0	value of a is put in register 2
	, Load (Imm 5) 3	-- 1	value of n is put in register 3
	, Load (Imm 1) 4	-- 2	value of power is put in register 4

	, Calc Eq 3 0 0		-- 3	Calculate n==0, and throw the result away
	, CJump 8		-- 4	If True (ie cnd==1), then got EndProg
	, Calc Mul 2 4 4	-- 5	multiply a with power, give the result to power
	, Calc Decr 3 0 3	-- 6	Decrement n with 1
	, Jump 3		-- 7	Go back to instruction 3
	, EndProg		-- 8
	]


{-----------------------------------
| Example 2
------------------------------------

Calculate the "3n+1" function "three"

Haskell definition:
-}

three :: Int -> [Int]
three n	| n == 1		= []
	| n `mod` 2 == 0	= n : three (n `div` 2)
	| otherwise		= n : three (3*n +1)


{-
Below: prog2 produces a comparable result.

BE AWARE: the same remarks apply as above, i.e.: prog2 is written BY HAND,
and is NOT the result of a compiler.

As start value for n is chose 7, and put in register 5.
In registers 1,2,3 the constants 1,2,3 are put (respectively), for practical reasons:
then the content of the register is the same as its address, which makes the code
below easier to read.
This will NEVER be done like that by a compiler.
-----------------------------------}

prog2 = [ Load (Imm 7) 5	-- 0	Load initial value of n (7) in register 5

	, Load (Imm 1) 1	-- 1	Load constant 1 in register 1
	, Load (Imm 2) 2	-- 2	Load constant 2 in register 2
	, Load (Imm 3) 3	-- 3	Load constant 3 in register 3

	, Calc Eq 5 1 0		-- 4	Calculte n==1, and throw result away (register 0)
				--	This sets the cnd-register to 0 (False) or 1 (True)
	, CJump 13		-- 5	If cnd=1, then we're done, and thus go to EndProg
	, Calc Mod 5 2 0	-- 6	Otherwise: calculate n`mod`2, and throw the result away.
				--	Again, cnd may be 0 (if n`mod`2=0) or 1 (otherwise)
	, CJump 10		-- 7	If cnd=1 (i.e: if n is odd), then go to 10 ...
	, Calc Div 5 2 5	-- 8	... else divide n by 2 and put the result in register 5.
	, Jump 4		-- 9	Jump back to instruction 4.
	, Calc Mul 5 3 5	-- 10	At this point n is odd, thus multiply by 3 ...
	, Calc Add 5 1 5	-- 11	... and add 1.
	, Jump 4		-- 12	Jump back to 4.
	, EndProg		-- 13	End of Program.
	]



{-----------------------------------
| Example 3
------------------------------------

Calculation of an expression with a stack.

Calculate:

((3*4)+(5*6))+(7*8)

Note: this program CAN be generated by a compiler.
The end-result of the expression will be in register 1.
-----------------------------------}


prog3 = [ Load (Imm 3) 1
	, Load (Imm 4) 2
	, Calc Mul 1 2 1

	, Load (Imm 5) 2
	, Load (Imm 6) 3
	, Calc Mul 2 3 2
	, Calc Add 1 2 1

	, Load (Imm 7) 2
	, Load (Imm 8) 3
	, Calc Mul 2 3 2
	, Calc Add 1 2 1
	, EndProg
	]


{-- Programs that should be compilable -}

vierkeervier = [
    (Assign (Var 'a') (Const 4)),
    (Assign (Var 'b') (Const 4)),
    (Assign (Var 'r') (Const 0)),
    (While (N2 Gt (Var 'b') (Const 0)) [
        (Assign (Var 'r') (N2 Add (Var 'r') (Var 'a'))),
        (Assign (Var 'b') (N2 Sub  (Var 'b') (Const 1)))])]


complexMath = [Assign (Var 'a') (N2 Add (N2 Mul (Const 4) (Const 5)) (N2 Div (Const 10) (Const 2))), (Assign (Var 'b') (Const 3))]


ifelseexample = [
    (Assign (Var 'a') (Const 3)),
    (If 
        (N2 Lt (Var 'a') (Const 2)) 
            [(Assign (Var 'a') (Const 9))] 
            [(Assign (Var 'b') (Const 14))]
    )]

simpleexpression = [
    (Assign (Var 'a') (N2 Add (N2 Add (Const 4) (Const 5)) (N2 Add (Const 7) (Const 1))))]
    
    
ifElseLocalScope = [
    (Assign (Var 'a') (Const 3)),
    (If
        (N2 Lt (Var 'a') (Const 4))
            [(Assign (Var 'b') (Const 3)), (Assign (Var 'a') (Var 'b'))]
            []
    ),
    (Assign (Var 'c') (Var 'b')) -- Should raise an error, b undefined
    ]