zimport FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char
import Data.List

database :: [(String, Int, Char, String)]
database = [
            ("Lennart", 21, 'm', "Enschede"),
            ("Peter", 20, 'm', "Enschede"),
            ("Annie", 59, 'v', "Apeldoorn"),
            ("Frans", 20, 'm', "Amsterdam"),
            ("Vera", 34, 'v', "Groningen")
            ]

getName :: (String,Int,Char,String) -> String
getName (name, _, _, _) = name

getAge :: (String,Int,Char,String) -> Int
getAge (_, age, _, _) = age

getGender :: (String,Int,Char,String) -> Char
getGender (_, _, gender, _) = gender

getCity :: (String,Int,Char,String) -> String
getCity (_, _, _, city) = city

incAgeR :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeR [] _ = []
incAgeR (x:xs) n = (getName x, (getAge x) + n, getGender x , getCity x) : incAgeR xs n

incAgeL :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeL x n = [(getName i, (getAge i) + n, getGender i , getCity i) | i <- x]

incAgeM :: [(String,Int,Char,String)] -> Int -> [(String,Int,Char,String)]
incAgeM x n = map (\(name, age, gender, city) -> (name, age + n, gender, city)) x

getNameFemaleAge30to40R :: [(String, Int, Char, String)]  -> [(String, Int, Char, String)]
getNameFemaleAge30to40R [] = []
getNameFemaleAge30to40R (x:xs)
    | getAge x >= 30 && getAge x < 40 && getGender x == 'v' = x : getNameFemaleAge30to40R xs
    | otherwise = getNameFemaleAge30to40R xs

getNameFemaleAge30to40L :: [(String, Int, Char, String)]  -> [(String, Int, Char, String)]
getNameFemaleAge30to40L x = [(getName i, getAge i, getGender i , getCity i) | i <- x, getAge i >= 30, getAge i < 40, getGender i == 'v']

getNameFemaleAge30to40F :: [(String, Int, Char, String)]  -> [(String, Int, Char, String)]
getNameFemaleAge30to40F x = filter (\(name,age,gender,city) ->  age>=30 && age<40 && gender=='v') x

getAgeByName :: [(String, Int, Char, String)] -> String -> Int
getAgeByName [] s = -1
getAgeByName (x:xs) s
    | map toLower (getName x) == map toLower s = getAge(x)
    | otherwise = getAgeByName xs s


sortByAge ::  [(String,Int,Char,String)] ->  [(String,Int,Char,String)]
sortByAge xs = map swap (sort (map swap xs))

sortByAgeF :: [(String,Int,Char,String)] ->  [(String,Int,Char,String)]
sortByAgeF = map swap . sort . map swap 

swap (t0, t1, t2, t3) = (t1, t0, t2, t3)

