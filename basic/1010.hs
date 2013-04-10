main = interact (unwords . map show . mainwork . map read . words)

-- 注意零多项式，否则通不过 case 2&4
mainwork xs = let res = func xs
	in if null res then [0,0] else res

func [] = []
func (x:y:xs) = if y/=0 then (x*y):(y-1):func xs else func xs

