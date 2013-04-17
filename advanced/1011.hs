import Data.List(maximumBy)
import Data.Function(on)

main = interact mainwork

mainwork content = unwords (map fst choice) ++ ' ' : show profit
	where
		choice = map (maximumBy (compare `on` snd) . boxing . words) . lines $ content
		profit = round' ((product (map snd choice) * 0.65 - 1) * 2)

boxing :: [String] -> [(String, Double)]
boxing [a,b,c] = [("W", read a), ("T", read b), ("L", read c)]

round' :: Double -> Double
round' x = fromIntegral(round(x*100)) / 100

