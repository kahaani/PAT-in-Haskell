main = interact (show . mainwork . map read . tail . words)

mainwork = func 0

func :: Int -> [Int] -> Int
func _ [] = 0
func last (x:xs) = 5 + time + func x xs
	where time = if last > x then (last-x)*4 else (x-last)*6
