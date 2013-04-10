import Data.List(minimumBy, maximumBy)
import Data.Function(on)

main = getLine >> getContents >>= (putStr . mainwork)

mainwork contents = unwords [firstone, lastone]
	where
		logs = map (\[a,b,c] -> (a,(b,c))) . map words . lines $ contents
		firstone = fst . minimumBy (compare `on` (fst.snd)) $ logs
		lastone  = fst . maximumBy (compare `on` (snd.snd)) $ logs
