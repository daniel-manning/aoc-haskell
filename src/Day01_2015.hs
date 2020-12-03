module Day01_2015
    (
    day01Part1,
    day01Part2
    ) where

import Data.List (group, sort, find)
import Data.Maybe (fromJust)

evaluateFloorCommand :: String -> Int
evaluateFloorCommand input =
  snd (fromJust $ find (\l -> fst l == '(') list) - snd (fromJust $ find (\l -> fst l == ')') list)
  where
    list = map (\x -> (head x, length x)) $ group $ sort input


firstHitBasement = firstHitBasement' 0 0

firstHitBasement' :: Int -> Int -> [Char] -> Maybe Int
firstHitBasement' pos (-1) _ = Just pos
firstHitBasement' _ _ [] = Nothing
firstHitBasement' pos value (x:xs) | x == '(' = firstHitBasement' (pos + 1) (value + 1) xs
                                   | x == ')' = firstHitBasement' (pos + 1) (value - 1) xs
                                   | otherwise = Nothing

day01Part1 = evaluateFloorCommand <$> readString
day01Part2 = fromJust . firstHitBasement <$> readString

readString :: IO String
readString = readFile "resource/2015/day01"