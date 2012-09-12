import FPPrac
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
