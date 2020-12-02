module Day01
    (
    day01Pt1Solution,
    day01Pt2Solution
    ) where

day01Pt1Solution :: IO Int
day01Pt1Solution = head . map multiplyPair . findMatchingPair 2020 <$> readFileofInts

day01Pt2Solution :: IO Int
day01Pt2Solution = head . map multiplyTriple . findMatchingTriple 2020 <$> readFileofInts

multiplyPair :: (Int, Int) -> Int
multiplyPair (a, b) = a*b

multiplyTriple :: (Int, Int, Int) -> Int
multiplyTriple (a, b, c) = a * b * c

findMatchingPair:: (Num a, Ord a, Eq a) => a -> [a] -> [(a, a)]
findMatchingPair target list = filter (\l -> uncurry (+) l == target) $ crossList list

findMatchingTriple:: (Num a, Ord a, Eq a) => a -> [a] -> [(a, a, a)]
findMatchingTriple target list = filter (\l -> (fst3 l + snd3 l + thd3 l) == target) $ crossListTriple list

fst3::(a,b,c) -> a
fst3 (a, b, c) = a
snd3::(a,b,c) -> b
snd3 (a, b, c) = b
thd3::(a,b,c) -> c
thd3 (a, b, c) = c

crossList:: (Ord a) => [a] -> [(a, a)]
crossList list = [(x,y) | x <-list, y<-list, x <= y]

crossListTriple:: (Ord a) => [a] -> [(a, a, a)]
crossListTriple list = [(x,y,z) | x <- list, y <- list, x <= y, z <- list,  y <= z]

readFileofInts :: IO [Int]
readFileofInts = map read . lines <$> readFile "resource/day01"
