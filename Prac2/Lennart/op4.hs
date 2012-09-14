import FPPrac
import Prelude (String)
--import Prelude (Char)
import Prelude (Int)
import Data.Char

pyth :: Int -> [(Int, Int, Int)]
pyth n = [(a,b,c) | a <- [1..n], b <- [1..n], c <- [1..n], a ^ 2 + b ^ 2 == c ^ 2]

-- Deze functie gaat niet werken voor grote getallen, omdat het programma dan niet termineert --

pythdiff :: Int -> [(Int,Int,Int)]
pythdiff n = [(a,b,c) | a <- [1..n], b <- [a..n], c <- [1..n], a ^ 2 + b ^ 2 == c ^ 2]

-- TODO geen idee hoe ik hier de multiples uit haal :( --
