-- see basic-1002

import Data.List(intercalate)
import Data.Char(digitToInt)

main = getLine >>= (putStr.func)

func = intercalate " " . map english . show . sum . map digitToInt

english '0' = "zero"
english '1' = "one"
english '2' = "two"
english '3' = "three"
english '4' = "four"
english '5' = "five"
english '6' = "six"
english '7' = "seven"
english '8' = "eight"
english '9' = "nine"
