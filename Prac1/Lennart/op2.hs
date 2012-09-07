import FPPrac
import Data.Char
import Data.Int

codeer :: Char -> Char
codeer x = if x >= 'A' && x <= 'Z' then chr(((ord(x) - ord('A') + 3) `mod` 26) + ord('A')) else if x >= 'a' && x <= 'z' then chr(((ord(x) - ord('a') + 3) `mod` 26) + ord('a')) else x

gcodeer :: Int -> Char -> Char
gcodeer y x = if x >= 'A' && x <= 'Z' then chr(((ord(x) - ord('A') + y) `mod` 26) + ord('A')) else if x >= 'a' && x <= 'z' then chr(((ord(x) - ord('a') + y) `mod` 26) + ord('a')) else x
