module Day11_2024 (
) where

import Data.Ord (comparing)
import Data.List (sortBy, groupBy)

data Stone = Stone Integer deriving (Eq, Ord, Show)

readData = map (\n -> Stone $ read n). words <$> readFile "resource/2024/day11"

advance :: Stone -> [Stone]
advance (Stone 0) = [Stone 1]
advance (Stone n) | l `mod` 2 == 0 = [Stone (read $ take (l `div` 2) ns),  Stone (read $ drop (l `div` 2) ns)]
                  | otherwise = [Stone (n * 2024)]
    where
        ns = show n
        l = length ns

blink :: [Stone] -> [Stone]
blink ss = advance =<< ss

blinkNTimes n xs = (iterate blink xs) !! n

runPt1 = length . blinkNTimes 25 <$> readData
----

-- we don't care about the order as it doesnt matter
-- put stones into buckets and advance from there

advance' :: (Stone, Int) -> [(Stone, Int)]
advance' (s, n) = map (\s' -> (s', n)) $ advance s

reduceStones :: [(Stone, Int)] -> [(Stone, Int)]
reduceStones = map (\xs -> (fst $ head xs, sum $ map snd xs)) . groupBy (\n m -> fst n == fst m) . sortBy (comparing fst)

groupStones :: [Stone] -> [(Stone, Int)]
groupStones = reduceStones . map (\s -> (s, 1))

blink' :: [(Stone, Int)] -> [(Stone, Int)]
blink' sns =  reduceStones $ advance' =<< sns

blinkNTimes' n xs = (iterate blink' xs) !! n

runPt2 = sum . map snd . blinkNTimes' 75 . groupStones <$> readData