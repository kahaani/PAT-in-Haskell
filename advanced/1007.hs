main = getLine >> getLine >>= (putStr . mainwork)

mainwork contents = unwords [show sum', show (nums!!left), show (nums!!right)]
	where
		nums = map read . words $ contents
		(sum,left,right) = findMax (-1,0,length nums-1) (0,0,0) nums
		sum' = if sum == -1 then 0 else sum

findMax :: (Int,Int,Int) -> (Int,Int,Int) -> [Int] -> (Int,Int,Int)
findMax res _ [] = res
findMax (sum',left',right') (sum,left,right) (x:xs)
	| sum + x < 0    = findMax (sum',  left', right') (0,     right+1, right+1) xs
	| sum + x > sum' = findMax (sum+x, left,  right ) (sum+x, left,    right+1) xs
	| otherwise      = findMax (sum',  left', right') (sum+x, left,    right+1) xs

