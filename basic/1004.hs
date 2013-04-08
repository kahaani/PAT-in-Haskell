import Data.List(sortBy)
import Data.Function(on)

main = do
	_ <- getLine
	contents <- getContents
	putStr . mainwork $ contents

mainwork xs = top_name ++ ' ' : top_course ++ '\n' : bot_name ++ ' ' : bot_course
	where
		sorted = sortBy (compare `on` snd) . map (parse) . lines $ xs
		(top_name, top_course) = fst (last sorted)
		(bot_name, bot_course) = fst (head sorted)

parse :: String -> ((String,String),Int)
parse xs = ((name, course), read grade)
	where [name,course,grade] = words xs


