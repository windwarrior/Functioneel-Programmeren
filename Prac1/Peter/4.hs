import FPPrac
import Prelude (Int)
import Prelude (Float)

wortel1 :: Float -> Float -> Float -> Float
wortel1 a b c 	
	|(d >= 0) = ((-b+sqrt(d))/(2*a))
	| otherwise = error "discriminant negatief"
	where d = discr a b c
				
wortel2 :: Float -> Float -> Float -> Float
wortel2 a b c 	
	|(d >= 0) = ((-b-sqrt(d))/(2*a))
	| otherwise = error "discriminant negatief"
	where d = discr a b c
				
discr a b c = (b^2 - 4*a*c)