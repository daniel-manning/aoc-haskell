module Day04_2020
    (
    ) where

    import Data.List (sort)
    import Data.String.Utils (split, splitWs, startswith)

    runDocuments = map mergeTokeniseSort . groupBetweenBlankLines <$> readDocuments
    countValidDocuments = length . filter validate <$> runDocuments

    validate:: [String] -> Bool
    validate input = (8 == length input) || ((7 == length input) && (not $ any (startswith "cid:") input))

    mergeTokeniseSort :: [String] -> [String]
    mergeTokeniseSort input = sort $ splitWs $ foldr (\a b -> a ++ " " ++ b) "" input

    groupBetweenBlankLines :: [String] -> [[String]]
    groupBetweenBlankLines input = groupBetweenBlankLines'' input []

    groupBetweenBlankLines'' :: [String] -> [String] -> [[String]]
    groupBetweenBlankLines'' [] n = [n]
    groupBetweenBlankLines'' (x:xs) n | length x == 0 = n : groupBetweenBlankLines'' xs []
                                    | otherwise = groupBetweenBlankLines'' xs (x : n)

    readDocuments :: IO [String]
    readDocuments = lines <$> readFile "resource/2020/day04"