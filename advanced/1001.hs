main = interact mainwork

mainwork xs = format (a+b)
	where [a,b] = map read . words $ xs

format n = if n<0 then '-':res else res
	where
		res = splitBy3 (length str) str
		str = show $ abs n

splitBy3 _ [] = []
splitBy3 i (x:xs)
	| i > 3 && rem i 3 == 1 = x : ',' : splitBy3 (i-1) xs
	| otherwise = x : splitBy3 (i-1) xs

