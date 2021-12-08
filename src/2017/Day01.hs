module Day01 where

import Data.List (find)
import Data.Maybe (fromJust)

readFileofInts :: IO [Int]
readFileofInts = map (\x -> read [x]) <$> readFile "resource/2017/day01"

nextEqual :: Eq a => [a] -> [a]
nextEqual [] = []
nextEqual xs = nextIsEqual (head xs) xs
   where
       nextIsEqual k [] = []
       nextIsEqual k [x] | k == x = [x]
                         | otherwise = []
       nextIsEqual k (x : y : xs) | x == y = x : nextIsEqual k (y : xs)
                                  | otherwise = nextIsEqual k (y : xs)

runPt1 :: [Int] -> Int
runPt1 = sum . nextEqual
--------------------
addPosition :: (Num b, Enum b) => [a] -> [(a, b)]
addPosition xs = xs `zip` [0..]

findHalwayAlong :: (Eq a) => [(a, Int)] -> [(a, Int)] -> [a]
findHalwayAlong [] _ = []
findHalwayAlong _ [] = []
findHalwayAlong k (x:xs) | fst x == m = m : findHalwayAlong k xs
                         | otherwise = findHalwayAlong k xs
    where
        l = length k
        h = l `div` 2
        m = fst $ fromJust $ find (\g -> snd g == (h + snd x) `mod` l) k

runPt2 :: [Int] -> Int
runPt2 = sum . (\x -> findHalwayAlong x x) . addPosition

solution :: IO Int
solution = runPt2 <$> readFileofInts