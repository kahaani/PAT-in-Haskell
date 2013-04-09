import Data.Char(digitToInt)

main = getLine >>= (putStr.func)

func [g] = ['1'..g]
func [s,g] = replicate (digitToInt s) 'S' ++ ['1'..g]
func [b,s,g] = replicate (digitToInt b) 'B' ++ replicate (digitToInt s) 'S' ++ ['1'..g]

