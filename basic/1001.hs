main = interact(show . callatz . read)

callatz n
	| n == 1    = 0
	| odd n     = 1 + callatz (div (n*3+1) 2)
	| otherwise = 1 + callatz (div n 2)
