main =  interact (show . mainwork . read)

mainwork x = countPairs . takeWhile (<= x) $ primes

countPairs [] = 0
countPairs [x] = 0
countPairs (x:xs@(y:_)) = (if y-x == 2 then 1 else 0) + countPairs xs

primes = 2 : filter (isprime) [3,5..]
	where isprime n = null . filter (\x -> n `mod` x == 0) . takeWhile (\x -> x*x <= n) $ primes

-- alternative primes
-- primes = sieve [2..]
-- 	where sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]
