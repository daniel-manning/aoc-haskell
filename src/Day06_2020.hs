module Day06_2020
    (
    ) where

    import Data.List (nub)

    groupBetweenBlankLines :: [String] -> [[String]]
    groupBetweenBlankLines input = groupBetweenBlankLines'' input []

    groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | length x == 0 = n : groupBetweenBlankLines'' xs []
                                    | otherwise = groupBetweenBlankLines'' xs (x : n)

    runAnswers = sum <$> (map (length . nub . foldr (++) "") . groupBetweenBlankLines) <$> readAnswers

    readAnswers :: IO [String]
    readAnswers = lines <$> readFile "resource/2020/day06"
