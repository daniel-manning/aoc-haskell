module Day06_2023 (
) where

import Data.List.Extra (trim)

data Race = Race Int Int deriving Show
-------------
parseRace :: [String] -> [Race]
parseRace xs = zipWith Race times distance
     where
        times = map read $ words $ trim $ drop 5  $ head xs
        distance = map read $ words $ trim $ drop 9 (xs !! 1)

readData :: IO [Race]
readData = parseRace . lines <$> readFile "resource/2023/day06"

totalDistance :: Int -> Int -> Int
totalDistance totalTime holdTime = speed * (totalTime - holdTime)
    where
        speed = holdTime

strategies :: Race -> [Int]
strategies (Race t _) = map (totalDistance t) [0..t]

waysOfWinning :: Race -> Int
waysOfWinning r@(Race _ d)= length $ filter (> d) $ strategies r

runPt1 = product . map waysOfWinning <$> readData
--------------------
readData' :: IO Race
readData' = parseRace' . lines <$> readFile "resource/2023/day06"

parseRace' :: [String] -> Race
parseRace' xs = Race time distance
     where
        time = read $ concat $ words $ trim $ drop 5  $ head xs
        distance = read $ concat $ words $ trim $ drop 9 (xs !! 1)

strategies' :: Race -> [Int]
strategies' (Race t _) = map (totalDistance t) $ reverse [0..t]

findEndsOfLosingWays r@(Race t d) = t - start - end - 1
    where 
        start = t - length (dropWhile (<= d) $ strategies r)
        end = t - length (dropWhile (<= d) $ strategies' r)

runPt2 = findEndsOfLosingWays <$> readData'