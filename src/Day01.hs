module Day01
    (
    day01Pt1Solution
    ) where

day01Pt1Solution :: IO [Int]
day01Pt1Solution = map multiplyPair <$> (findMatchingPair 2020) <$> readFileofInts

multiplyPair :: (Int, Int) -> Int
multiplyPair (a, b) = a*b

findMatchingPair:: (Num a, Ord a, Eq a) => a -> [a] -> [(a, a)]
findMatchingPair target list = filter (\l -> (fst l + snd l) == target) $ crossList list

crossList:: (Ord a) => [a] -> [(a, a)]
crossList list = [(x,y) | x <-list, y<-list, x <= y]

readFileofInts :: IO [Int]
readFileofInts = map read <$> lines <$> readFile "resource/day01"