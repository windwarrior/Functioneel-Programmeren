import FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char
import Data.List

database :: [(String,Int,Char,String)]
database = 	[
						("Peter", 		20, 	'm', 	"Enschede"),
						("Pieter", 		24, 	'm', 	"Enschede"),
						("Jochem", 	21, 	'm', 	"Enschede"),
						("Erica", 		45, 	'f', 	"Hengelo"),
						("Annie", 		35, 	'f', 	"Antwerpen"),
						("Henk", 		32, 	'm', 	"Antwerpen")
					]

-- Getters
					
getName :: (String,Int,Char,String) -> String
getName (name, _, _, _) = name

getAge :: (String,Int,Char,String) -> Int
getAge (_, age, _, _) = age

getGender :: (String,Int,Char,String) -> Char
getGender (_, _, gender, _) = gender

getCity :: (String,Int,Char,String) -> String
getCity (_, _, _, city) = city

-- Age Increasing Functions

incAgeRec :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeRec [] _ = []
incAgeRec (x:xs) n = (getName x, (getAge x) + n, getGender x, getCity x) : incAgeRec xs n

incAgeList :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeList m n = [(getName x, (getAge x) + n, getGender x, getCity x) | x <- m]

incAgeFunc :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeFunc xs n = map (\(name, age, gender, city)->(name, age+n, gender, city) ) xs

-- Women Selector ages 30-40

getWomenRec :: [(String,Int,Char,String)] -> [(String,Int,Char,String)]
getWomenRec [] = []
getWomenRec (x:xs)
	| age >=30 && age <= 40 && gender=='f' = x : getWomenRec xs
	| otherwise = getWomenRec xs
	where 
		age = getAge x
		gender = getGender x

getWomenList :: [(String,Int,Char,String)] -> [(String,Int,Char,String)]
getWomenList m = [(getName x, getAge x, getGender x, getCity x) | x <- m, (getAge x) >=30 && (getAge x) <=40 && (getGender x)=='f']

getWomenFunc :: [(String,Int,Char,String)] -> [(String,Int,Char,String)]
getWomenFunc xs = filter (\(name,age,gender,city) ->  age>=30 && age<=40 && gender=='f') xs

-- Get Age By Name

getAgeByName ::  [(String,Int,Char,String)] -> String -> [Int]
getAgeByName xs name1 = map getAge (filter (\(name,age,gender,city) ->  (map toLower name1) == (map toLower name)) xs)

-- Sort By Age

sortByAge ::  [(String,Int,Char,String)] ->  [(String,Int,Char,String)]
sortByAge xs = map swap (sort (map swap xs))

swap (t0, t1, t2, t3) = (t1, t0, t2, t3)
