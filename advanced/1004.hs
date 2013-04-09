import Data.List(intercalate)
import qualified Data.Map as Map

main = do
	_ <- getLine
	contents <- getContents
	putStr $ mainwork contents

mainwork contents = intercalate " " . map show $ count
	where
		childrenmap = getChildrenMap contents
		tree = buildTree childrenmap "01"
		count = countLeaves tree

type Element = String
type ChildrenMap = Map.Map Element [Element]

getChildrenMap :: String -> ChildrenMap
getChildrenMap = Map.fromList . map (helper . words) . lines
	where helper (x:_:xs) = (x,xs)

data Tree = Tree Element [Tree]

buildTree :: ChildrenMap -> Element -> Tree
buildTree childmap element = case Map.lookup element childmap of
		Nothing -> Tree element []
		Just xs -> Tree element (map (buildTree childmap) xs)

countLeaves :: Tree -> [Int]
countLeaves (Tree x xs) = if (null xs) then [1] else 0 : foldl1 zipTwoList (map countLeaves xs)

zipTwoList :: [Int] -> [Int] -> [Int]
zipTwoList [] ys = ys
zipTwoList xs [] = xs
zipTwoList (x:xs) (y:ys) = (x+y) : zipTwoList xs ys

