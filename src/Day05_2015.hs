module Day05_2015
    (
    ) where

    import Data.List (group, isInfixOf, tails)

    slide :: Int -> [a] -> [[a]]
    slide _ [] = []
    slide n xs | n >= length xs = [xs]
               | otherwise      = take n xs : slide n (drop 1 xs)

    containsRepeatsWithSeparator :: String -> Bool
    containsRepeatsWithSeparator input = any (\xs -> head xs == (head $ drop 2 xs)) $ slide 3 input

    twoLettersRepeatedNotOverLapped :: String -> Bool
    twoLettersRepeatedNotOverLapped input = any (\xs -> isInfixOf (take 2 xs) (drop 2 xs)) $ takeWhile (\l -> length l >= 4) $ tails input

    newRulesValidation input = all (\p -> p input) [containsRepeatsWithSeparator, twoLettersRepeatedNotOverLapped]

    containsAtLeast3Vowels :: String -> Bool
    containsAtLeast3Vowels input = 3 <= (length $ filter (\c -> c `elem` "aeiou") input)

    letterTwiceInARow :: String -> Bool
    letterTwiceInARow input = 1 <= (length $ filter (\x -> length x >= 2) $ group input)

    notContainForbiddenSubstring :: String -> Bool
    notContainForbiddenSubstring input = not $ any (\n -> isInfixOf n input) ["ab","cd","pq","xy"]

    validates :: String -> Bool
    validates input = all (\p -> p input) [containsAtLeast3Vowels, letterTwiceInARow, notContainForbiddenSubstring]

    day05Part1 = length . filter validates <$> readStrings
    day05Part2 = length . filter newRulesValidation <$> readStrings

    readStrings :: IO [String]
    readStrings = lines <$> readFile "resource/2015/day05"