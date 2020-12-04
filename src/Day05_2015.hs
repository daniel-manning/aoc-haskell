module Day05_2015
    (
    ) where

    import Data.List (group, isSubsequenceOf)

    containsAtLeast3Vowels :: String -> Bool
    containsAtLeast3Vowels input = 3 <= (length $ filter (\c -> c `elem` "aeiou") input)

    letterTwiceInARow :: String -> Bool
    letterTwiceInARow input = 1 <= (length $ filter (\x -> length x >= 2) $ group input)

    notContainForbiddenSubstring :: String -> Bool
    notContainForbiddenSubstring input = not $ any (\n -> isSubsequenceOf n input) ["ab","cd","pq","xy"]

    validates :: String -> Bool
    validates input = all (\p -> p input) [containsAtLeast3Vowels, letterTwiceInARow, notContainForbiddenSubstring]

    day05Part1 = filter validates <$> readStrings

    readStrings :: IO [String]
    readStrings = lines <$> readFile "resource/2015/day05"