module Day05_2023 (
) where

import ListUtils (groupBetweenBlankLines)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap (fromList, union, empty, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.List.Extra (headDef)

data Filter = Filter Int Int Int deriving Show
-------------
readData = parseAlmanac . lines <$> readFile "resource/2023/day05"

parseAlmanac :: [String] -> ([Int], [[[Int]]])
parseAlmanac xs = (parseSeeds (head xs), parseMaps (drop 2 xs))

parseSeeds :: String -> [Int]
parseSeeds = map read . words . drop 7

parseMaps :: [String] -> [[[Int]]]
parseMaps = map (map (map read . words). tail . reverse) . groupBetweenBlankLines
--------------
constructFilter [d,s,l] = Filter d s l

constructMap :: [[Int]] -> [Filter]
constructMap = map constructFilter

mapWithFilter :: Int -> Filter -> Maybe Int
mapWithFilter m (Filter d s l) = if m >= s && m < (s + l) then Just (d + (m - s)) else Nothing

siftLowerResult :: [Filter] ->Int ->  Int
siftLowerResult ms m = headDef m $ mapMaybe (mapWithFilter m) ms

siftWithMap :: [Int] -> [[Int]] -> [Int]
siftWithMap s m = map (siftLowerResult (constructMap m)) s

findNextNumbers ::  [Int] -> [[[Int]]] -> [Int]
findNextNumbers = foldl siftWithMap

runPt1 = minimum . uncurry findNextNumbers <$> readData

