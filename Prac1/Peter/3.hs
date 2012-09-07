import FPPrac
import Data.Char
import Prelude (Int)
import Prelude (Float)

saldo :: Int -> Float -> Float -> Float
saldo 0 b r = b
saldo n b r = saldo (n-1) (b*(1+(r/100))) r