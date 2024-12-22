module Day05_2023 (
) where

import ListUtils (groupBetweenBlankLines)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap (fromList, union, empty, lookup)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.List.Extra (headDef)
import Data.List.Split (chunksOf)
import Data.List (sortBy)
import Data.Ord (comparing)

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

siftLowerResult :: [Filter] -> Int ->  Int
siftLowerResult ms m = headDef m $ mapMaybe (mapWithFilter m) ms

siftWithMap :: [Int] -> [[Int]] -> [Int]
siftWithMap s m = map (siftLowerResult (constructMap m)) s

findNextNumbers ::  [Int] -> [[[Int]]] -> [Int]
findNextNumbers = foldl siftWithMap

runPt1 = minimum . uncurry findNextNumbers <$> readData
------------------
data Range = Range { source::Int, length::Int } deriving (Eq, Ord, Show)
sortByStart = sortBy (comparing source)


constructRange [s,r] = Range s r

turnSeedsToRanges :: [Int] -> [Range]
turnSeedsToRanges xs = map constructRange $ chunksOf 2 xs

adjacentRanges :: Range -> Range -> Maybe Range
adjacentRanges (Range a la) (Range b lb) | b < a = adjacentRanges (Range a la) (Range b lb)
                                         | a + la == b = Just (Range a (la + lb))
                                         | otherwise = Nothing

joinRanges :: [Range] -> [Range]
joinRanges = joinRanges' . sortByStart  
joinRanges' [] = []
joinRanges' [x] = [x]
joinRanges' (x : y : xs) = case adjacentRanges x y of
                            Nothing  -> x : joinRanges' (y:xs)
                            Just (Range a l) -> joinRanges' ( (Range a l) : xs )

mapRangesWithFilter :: Filter -> Range -> (Maybe Range, Maybe [Range])
mapRangesWithFilter (Filter d s l) (Range a ar) | a + ar < s = (Nothing, Just [(Range a ar)]) -- too low
                                          | a > s + l  = (Nothing, Just [(Range a ar)]) -- too high
                                          | a >= s && a + ar <= s + l = (Just (Range (a - s + d) ar), Nothing) --completely inside
                                          | a < s && a + ar <= s + l = (Just (Range d (ar - s + a)), Just [(Range a (s - a))]) -- hangs left
                                          | a >= s && a + ar > s + l = (Just (Range (d + a - s) (s + l - a)), Just [(Range (s + l) (ar - s - l + a))]) -- hangs right
                                          | otherwise = (Just (Range d l), Just [(Range a (s - a)), (Range (s+l) (ar + a - s - l))]) -- hangs both sides

passSeedsThroughFilter :: [Range] -> Filter -> ([Range], [Range])
passSeedsThroughFilter rs f = (\(mr, mrs) -> (catMaybes mr, concat $ catMaybes mrs)) $ unzip $ map (mapRangesWithFilter f) rs

foldThroughMap :: [Range] -> [Filter] -> [Range]
foldThroughMap rs [] = rs
foldThroughMap rs (f:fs) =  passed ++ foldThroughMap leftOver fs 
    where 
        (passed, leftOver) = passSeedsThroughFilter rs f

passThroughSieve :: [Range] -> [[Filter]] -> [Range]
passThroughSieve = foldl foldThroughMap

runPt2 = source . minimum . uncurry passThroughSieve . (\(s, fs) -> (turnSeedsToRanges s, map constructMap fs)) <$> readData