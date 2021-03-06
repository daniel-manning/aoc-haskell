module Day06_2020
    (
    ) where

    import Data.List (nub, foldl1')
    import Data.Set (Set)
    import qualified Data.Set as Set

    groupBetweenBlankLines :: [String] -> [[String]]
    groupBetweenBlankLines input = groupBetweenBlankLines'' input []

    groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | null x = n : groupBetweenBlankLines'' xs []
                                      | otherwise = groupBetweenBlankLines'' xs (x : n)

    day06pt1 = sum . map (length . nub . concat) . groupBetweenBlankLines <$> readAnswers
    day06pt2 = sum . map (length . elementsInAllLists) . groupBetweenBlankLines <$> readAnswers


    elementsInAllLists :: (Ord a) => [[a]] -> [a]
    elementsInAllLists list = Set.toList $ foldl1' Set.intersection $ map Set.fromList list

    readAnswers :: IO [String]
    readAnswers = lines <$> readFile "resource/2020/day06"
