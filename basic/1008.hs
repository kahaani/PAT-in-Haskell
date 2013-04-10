main = do
	fstline <- getLine
	sndline <- getLine
	putStr $ mainwork fstline sndline

mainwork fstline sndline = unwords $ rotate (map read . words $ fstline) (words sndline)

rotate [len,n] xs = take len . drop (len - (n `mod` len)) . cycle $ xs

