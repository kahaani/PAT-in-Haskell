--解题关键：如果a能覆盖b，那么b肯定不能覆盖a

import Data.List((\\),sortBy)
import Data.List(intercalate)

main = do
	_ <- getLine
	contents <- getLine
	putStr . mainwork $ contents

mainwork = intercalate " " . map show . sortBy (flip compare) . uniqueCallatz . (,) [] . map read . words

uniqueCallatz (xs, []) = xs
uniqueCallatz (xs, (y:ys)) = uniqueCallatz (y:(xs\\res), (ys\\res))
	where res = tail $ getCallatz y

getCallatz 1 = []
getCallatz n = n : if odd n then getCallatz (div (n*3+1) 2) else getCallatz (div n 2)
