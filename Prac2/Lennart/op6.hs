import FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char

deellijst :: [Number] -> [Number] -> [Number]
deellijst x y =  [drop j y | j <- [0..2]]
