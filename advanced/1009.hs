-- see advanced-1002

import Data.List(sortBy, groupBy)
import Data.Function(on)

main = do
	a <- getLine
	b <- getLine
	putStr $ mainwork a b

mainwork a b = unwords . (:) prefix . map (\(a,b) -> unwords [show a, show(round' b)]) $ res
	where
		opa = parse . tail . words $ a
		opb = parse . tail . words $ b
		tmp = [ (a1+b1,a2*b2) | (a1,a2)<-opa, (b1,b2)<-opb] -- only change here
		res = merge tmp
		prefix = show . length $ res

parse :: [String] -> [(Int,Double)]
parse [] = []
parse (x:y:xs) = (read x, read y) : parse xs

merge :: [(Int,Double)] -> [(Int,Double)]
merge = filter (\x -> snd x /= 0) . map (newadd) . groupBy ((==) `on` fst) . sortBy ((flip compare) `on` fst)

newadd :: [(Int,Double)] -> (Int,Double)
newadd = foldl1 (\(a,b) (_,c) -> (a,b+c))

-- be accurate to 1 decimal place, rounding half up
round' :: Double -> Double
round' x = fromIntegral(round(x*10)) / 10

