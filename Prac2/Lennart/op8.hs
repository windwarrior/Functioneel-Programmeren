import FPPrac
import Prelude (String)
import Prelude (Int)
import Data.Char
import Data.List

minmax :: [Int] -> [Int]
minmax [] = []
minmax x = (minimum (x) : minmax(x \\ [minimum (x), maximum (x)])) ++ maximum (x):[]  
