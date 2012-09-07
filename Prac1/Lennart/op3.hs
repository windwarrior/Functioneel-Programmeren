import FPPrac
import Data.Char
import Data.Int
import Prelude (Float)

saldo :: Int -> Float -> Float -> Float
saldo n b r 
    | n > 0 = saldo (n-1) (b * (1 + (r/100))) r
    | otherwise = b 
