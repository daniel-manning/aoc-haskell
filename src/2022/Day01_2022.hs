module Day01_2022 (
) where

import Data.List (sortBy)

groupBetweenBlankLines :: Foldable t => [t a] -> [[t a]]
groupBetweenBlankLines input = map reverse $ groupBetweenBlankLines'' input []

groupBetweenBlankLines'' :: Foldable t => [t a] -> [t a] -> [[t a]]
groupBetweenBlankLines'' [] n = [n]
groupBetweenBlankLines'' (x:xs) n | null x = n : groupBetweenBlankLines'' xs []
                                  | otherwise = groupBetweenBlankLines'' xs (x : n)

readData :: IO [[Int]]
readData = map (map read) . groupBetweenBlankLines . lines <$> readFile "resource/2022/day01"

runPt1 = maximum . map sum <$> readData

sortDescending :: Ord a => [a] -> [a]
sortDescending = sortBy (flip compare)

runPt2 =  sum . take 3 . sortDescending . map sum <$> readData