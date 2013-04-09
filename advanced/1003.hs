import Data.List(sortBy)
import Data.Function(on)
import qualified Data.Map as Map

main = do
	basic <- getLine
	teams <- getLine
	loads <- getContents
	putStr $ mainwork basic teams loads

mainwork basic teams loads = show len ++ ' ' : show teamnum
	where
		[citynum, _, source, dest] = getBasicInfo basic --assert: source==dest works normally
		teammap  = getTeamMap teams
		loadmap  = getLoadMap loads
		cityinfo = getCityInfo citynum source
		destnode = shortestPath loadmap dest cityinfo 
		len      = length . getPaths $ destnode --assert: len>0
		teamnum  = maximum . map (sum . map ((!!) teammap)) . getPaths $ destnode

type CityNum = Int
type TeamNum = Int
type CityId  = Int
type Length  = Int

getBasicInfo :: String -> [CityId]
getBasicInfo = map read. words

getTeamMap :: String -> [TeamNum]
getTeamMap = map read . words

type LoadMap = Map.Map (CityId,CityId) Length

getLoadMap :: String -> LoadMap
getLoadMap = Map.fromList . concat . map (helper . map read . words) . lines
	where helper [a,b,c] = [((a,b),c),((b,a),c)] --assert: c>0

data CityInfo = CityInfo {
		getCityId :: CityId,
		getLength :: Length,
		getPaths  :: [[CityId]]
	}

getCityInfo :: CityNum -> CityId -> [CityInfo]
getCityInfo citynum source = map helper [0..(citynum-1)]
	where helper curr = if (curr == source) then CityInfo curr 0 [[curr]] else CityInfo curr (maxBound::Length) []

shortestPath :: LoadMap -> CityId -> [CityInfo] -> CityInfo
shortestPath loadmap target cities =
	if (target == getCityId curr) then curr else shortestPath loadmap target result
	where
		(curr : rest) = sortBy (compare `on` getLength) $ cities --minimumBy
		--assert: getLength curr /= (maxBound::Length)
		result = map (checkPair loadmap curr) rest

checkPair :: LoadMap -> CityInfo -> CityInfo -> CityInfo
checkPair loadmap lh rh = case Map.lookup (getCityId lh, getCityId rh) loadmap of
		Nothing  -> rh
		Just len -> case (compare (getLength lh + len) (getLength rh)) of
			GT -> rh
			LT -> CityInfo (getCityId rh) (getLength lh + len) (map ((:) (getCityId rh)) (getPaths lh))
			EQ -> CityInfo (getCityId rh) (getLength rh) (map ((:) (getCityId rh)) (getPaths lh) ++ getPaths rh)

