module Day15_2020
    (
    ) where

    import Data.List (elemIndex)
    import Data.Maybe (fromMaybe)

    --reverse the history list
    considerLastNumberSpoken :: [Int] -> [Int]
    considerLastNumberSpoken (x:xs) = (fromMaybe 0 ( (+1) <$> (x `elemIndex` xs))):(x:xs)

    --input = [0,3,6]
    input = [7,12,1,0,16,2]
    day15Pt1 = head $ last $ take (2020 - (length input) + 1) $ iterate considerLastNumberSpoken $ reverse input