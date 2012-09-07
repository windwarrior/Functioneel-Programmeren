import FPPrac

myfilter :: (a->Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) 
	|  f x 				= x: myfilter f xs
	| otherwise 		= myfilter f xs
	
myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f a [] = a
myfoldl f a (x:xs) =  myfoldl f (f a x) xs

myfoldr :: (b -> a -> a) -> a -> [b] -> a
myfoldr f a [] = a
myfoldr f a (x:xs) =  f x (myfoldr f a xs)

myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myzipWith f [] _ = []
myzipWith f _ [] = []
myzipWith f (x:xs) (y:ys)  = (f x y) : myzipWith xs ys