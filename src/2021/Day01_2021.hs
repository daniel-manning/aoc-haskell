module Day01_2021
    (
    ) where

import Text.Read
import Data.Maybe

compareDepths :: (Int, Int) -> Int -> (Int, Int)
compareDepths (oldDepth, noOfDeeper) depth  | depth > oldDepth = (depth, noOfDeeper + 1)
                                            | otherwise = (depth, noOfDeeper)

noOfIncreasingDepths :: [Int] -> Int
noOfIncreasingDepths xs = (-1) + snd (foldl compareDepths (0, 0) xs)

slidingWindow :: Int -> [Int] -> [[Int]]
slidingWindow n xs | n > length xs  = []
                   | otherwise = take n xs  : slidingWindow n (tail xs)

runPt2 :: [String] -> Int
runPt2 = noOfIncreasingDepths . map sum . slidingWindow 3 . map (fromJust . readMaybe)

runPt1 :: [String] -> Int
runPt1 = noOfIncreasingDepths . map (fromJust . readMaybe)

main :: IO Int
main = runPt2 . lines <$> readFile "resource/2021/day01"