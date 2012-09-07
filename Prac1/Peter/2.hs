import FPPrac
import Data.Char
import Prelude (Int)

codeer :: Char -> Char
codeer x 	| (x< 'x' && x>='a') || (x< 'X' && x>='A') = chr(ord(x)+3)
			| (x>='x' && x<='z') || (x>='X' && x<='Z') = chr(ord(x)-23)
			| otherwise = x

gcodeer :: Int -> Char -> Char
gcodeer y x | (x<=chr(ord('z')-y) && x>='a') || (x<=chr(ord('Z')-y) && x>='A') = chr(ord(x)+y)
			| (x> chr(ord('z')-y) && x<='z') || (x> chr(ord('Z')-y) && x<='Z') = chr(ord(x)-(26-y))
			| otherwise = x