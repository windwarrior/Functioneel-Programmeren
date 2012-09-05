import FPPrac
import Prelude (Float)

extrX :: Float -> Float -> Float -> Float
extrX a b c = (-b / (2 * a))

extrY :: Float -> Float -> Float -> Float
extrY a b c = a * x ^ 2 + b * x + c
    where x = extrX a b c
