module Day05_2015
    (
    ) where

    import Data.List (group, isInfixOf, tails)

    slide :: Int -> [a] -> [[a]]
    slide _ [] = []
    slide n xs | n >= length xs = [xs]
               | otherwise      = take n xs : slide n (drop 1 xs)

    containsRepeatsWithSeparator :: String -> Bool
    containsRepeatsWithSeparator input = any (\xs -> head xs == (xs !! 2)) $ slide 3 input

    twoLettersRepeatedNotOverLapped :: String -> Bool
    twoLettersRepeatedNotOverLapped input = any (\xs -> take 2 xs `isInfixOf` drop 2 xs) $ takeWhile (\l -> length l >= 4) $ tails input

    newRulesValidation input = all (\p -> p input) [containsRepeatsWithSeparator, twoLettersRepeatedNotOverLapped]

    containsAtLeast3Vowels :: String -> Bool
    containsAtLeast3Vowels input = 3 <= length (filter (`elem` "aeiou") input)

    letterTwiceInARow :: String -> Bool
    letterTwiceInARow input = any (\ x -> length x >= 2) (group input)

    notContainForbiddenSubstring :: String -> Bool
    notContainForbiddenSubstring input = not $ any (`isInfixOf` input) ["ab","cd","pq","xy"]

    validates :: String -> Bool
    validates input = all (\p -> p input) [containsAtLeast3Vowels, letterTwiceInARow, notContainForbiddenSubstring]

    day05Part1 = length . filter validates <$> readStrings
    day05Part2 = length . filter newRulesValidation <$> readStrings

    readStrings :: IO [String]
    readStrings = lines <$> readFile "resource/2015/day05"