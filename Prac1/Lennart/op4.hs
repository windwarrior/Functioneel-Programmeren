import FPPrac
import Prelude (Float)

wortel1 :: Float -> Float -> Float -> Float
wortel1 a b c
    | disc a b c >= 0 = (-1 * b + sqrt (d)) / 2 * a
    | otherwise = error "discriminant negatief"
    where d = disc a b c

wortel2 :: Float -> Float -> Float -> Float
wortel2 a b c
    | disc a b c >= 0 = (-1 * b - sqrt (d)) / 2 * a
    | otherwise = error "discriminant negatief"
    where d = disc a b c

disc :: Float -> Float -> Float -> Float
disc a b c = b ^ 2 - 4 * a * c
 
