import Data.List(intercalate)
import Data.Char(digitToInt)

main = getLine >>= (return.func) >>= putStrLn

func = intercalate " " . map pinyin . show . sum . map digitToInt

pinyin '0' = "ling"
pinyin '1' = "yi"
pinyin '2' = "er"
pinyin '3' = "san"
pinyin '4' = "si"
pinyin '5' = "wu"
pinyin '6' = "liu"
pinyin '7' = "qi"
pinyin '8' = "ba"
pinyin '9' = "jiu"
