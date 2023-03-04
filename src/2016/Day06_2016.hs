module Day06_2016 (
) where


import Data.List (groupBy, sortBy, sort, group, sortOn)
import Data.Function (on)
import Data.Ord (Down(..))

readData :: IO [String]
readData = lines <$> readFile "resource/2016/day06"

------------
groupByListPosition :: [[a]] -> [[a]]
groupByListPosition = map (map snd) . groupBy (\a b -> fst a == fst b) . sortBy (compare `on` fst) . concatMap (zip [0..])

mostPopular :: (Ord a) => [a] -> a
mostPopular = head . map snd . sortOn (Down . fst) . map (\x -> (length x, head x)) . group . sort

findMostRepeated :: [String] -> String
findMostRepeated = map mostPopular . groupByListPosition

runPt1 = findMostRepeated <$> readData
---------
leastPopular :: (Ord a) => [a] -> a
leastPopular = last . map snd . sortOn (Down . fst) . map (\x -> (length x, head x)) . group . sort

findLeastRepeated :: [String] -> String
findLeastRepeated = map leastPopular . groupByListPosition

runPt2 = findLeastRepeated <$> readData
