module Day11_2015
    (
        day11Pt1,
        day11Pt2
    ) where

    import Data.Char (digitToInt, ord, chr)
    import Data.List (isInfixOf, nub, group, foldl')
    import ListUtils (window)

    orderedLetters = ['a'..'z']

    hasAStraightOfLetters input =  any (`isInfixOf` orderedLetters) $ window 3 input
    doesNotContainIOL input =  all (`notElem` input) "iol"
    contains2DifferentNotOverlappingPairs input = 2 <= length (filter (\(n, _) -> n >= 2) $ map (\x -> (length x, head $ nub x)) $ group input)

    validate input = all (\f -> f input) [hasAStraightOfLetters, doesNotContainIOL, contains2DifferentNotOverlappingPairs]

    fromChar x = ord x - 96
    toChar n = chr (n + 96)

    passwordIntFormat x = foldl' (\a b -> 27*a + b) 0 (map fromChar x)

    intToPassword n = reverse $ intToPassword'' n
    intToPassword'' n | n <= 27 = [toChar ( (n `mod` 27))]
                      | n `mod` 27 == 0 = intToPassword'' (n `div` 27)
                      | otherwise = toChar ( (n `mod` 27)) : intToPassword'' (n `div` 27)


    incrementString x =  reverse . incrementString'' $ reverse x

    incrementString'' [] = "a"
    incrementString'' ('z': xs) = 'a': incrementString'' xs
    incrementString'' (c: xs) =  toChar (1 + fromChar c):xs
   
    nextPassword :: String -> String
    nextPassword input = head $ filter validate $ tail $ iterate incrementString input

    day11Pt1 = nextPassword "hxbxwxba"
    day11Pt2 = nextPassword "hxbxxyzz"