import FPPrac

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f [] = []
myfilter f (x:xs) 
    | f x = x : myfilter f xs
    | otherwise =  myfilter f xs

myfoldl :: (a -> b -> a) -> a -> [b] -> a
myfoldl f z []= z
myfoldl f z (x:xs) = myfoldl f (f z x) xs
    
myfoldr :: (b -> a -> a) -> a -> [b] -> a
myfoldr f z [] = z
myfoldr f z (x:xs) = f (x) (myfoldr f z xs )

--myzipWith :: (a -> b -> c) -> [a] -> [b] -> [c] 
