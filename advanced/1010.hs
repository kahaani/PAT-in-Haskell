import Data.Char(ord)
import Data.Int(Int16,Int32,Int64)

type IntX = Integer

main = interact mainwork

mainwork contents = case res of
		Nothing -> "Impossible"
		Just x  -> show x
	where
		[a',b',tag',radix'] = words contents

		(a,b)  = if tag == 1 then (a',b') else (b',a')
		tag    = read tag'
		radix  = read radix' :: IntX

		numa   = parseInt (map digitToInt a) radix
		numbs  = map digitToInt b
		lbound = max 2 (maximum numbs + 1)
		rbound = fromIntegral (maxBound :: Int32)  -- for case 7
		res    = binarySearchRadix numbs numa lbound rbound

-- cannot pass case 7 using linear search
binarySearchRadix :: [IntX] -> IntX -> IntX -> IntX -> Maybe IntX
binarySearchRadix nums target lbound rbound = 
	if lbound > rbound
	then Nothing
	else
		if lboundV == target
		then Just lbound
		else case compare middleV target of
			EQ -> binarySearchRadix nums target lbound middle
			LT -> binarySearchRadix nums target (middle+1) rbound
			GT -> binarySearchRadix nums target lbound (middle-1)
		where
			middle  = div (lbound + rbound) 2
			--middle  = lbound + div (rbound - lbound) 2
			middleV = parseInt nums middle
			lboundV = parseInt nums lbound

parseInt :: [IntX] -> IntX -> IntX
parseInt xs r = foldl1 (\acc x -> acc * r + x) xs

digitToInt :: Char -> IntX
digitToInt x
	| x>='0' && x<='9' = fromIntegral (ord x - ord '0')
	| x>='a' && x<='z' = fromIntegral (ord x - ord 'a' + 10)

