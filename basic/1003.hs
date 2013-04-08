main = do
	_ <- getLine
	contents <- getContents
	mapM_ putStrLn . map (mainwork) . lines $ contents

-- alternative main function:
-- import Data.List(intercalate)
-- main = interact (intercalate "\n".map mainwork.tail.lines)

mainwork = msg.check

msg True  = "YES"
msg False = "NO"

check xs = flag && (a*(b+1)) == c
	where (a,b,c,flag) = isValid xs 0

-- DFA
isValid [] 3 = (0,0,0,True)
isValid [] _ = (0,0,0,False)
isValid (x:xs) state = case (x,state) of
	('A',0) -> let (a,b,c,flag) = isValid xs 0 in (1+a,b,  c,  flag)
	('P',0) -> let (a,b,c,flag) = isValid xs 1 in (a,  b,  c,  flag)
	('A',1) -> let (a,b,c,flag) = isValid xs 2 in (a,  b,  c,  flag)
	('A',2) -> let (a,b,c,flag) = isValid xs 2 in (a,  1+b,c,  flag)
	('T',2) -> let (a,b,c,flag) = isValid xs 3 in (a,  b,  c,  flag)
	('A',3) -> let (a,b,c,flag) = isValid xs 3 in (a,  b,  1+c,flag)
	otherwise -> (0,0,0,False)

