import FPPrac
import Data.Int

-- De practicumhandleiding noemt hem 'r' dat vond ik onhandig
rlist :: Number -> Number -> [Number]
rlist s v = s : rlist (s+v) v

-- De practicumhandleiding noemt hem 'r1' dat vond ik onhandig
-- opzich kan dit met head, of met de !! operator, dan is het drop deel niet nodig
getn :: Number -> Number -> Number -> Number
getn s v i = head (drop (i - 1) (take i (rlist s v)))

-- Hier heb ik gebruik gemaakt van de ingebouwde sum methode, omdat ik in opgave 6 al bewezen heb
-- een lijst te kunnen optellen
getsum :: Number -> Number -> Number -> Number -> Number
getsum s v i j = sum( drop (i - 1) (take j (rlist s v)))
