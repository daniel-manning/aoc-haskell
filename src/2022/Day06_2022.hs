module Day06_2022 (
) where

import qualified Data.Set as Set

readData = lines <$> readFile "resource/2022/day06"
-------------
sliding :: Int -> [a] -> [[a]]
sliding n [] = []
sliding n xs = take n xs : sliding n (tail xs)

allItemsDistinct :: (Ord a) => [a] -> Bool
allItemsDistinct xs = length xs == Set.size (Set.fromList xs)

packetMarkerEndInBuffer :: String -> Int
packetMarkerEndInBuffer xs = 4 + fst (head $ filter (allItemsDistinct. snd) $ zip [0..] (sliding 4 xs))

runPt1 = map packetMarkerEndInBuffer <$> readData
----------------
messageMarkerEndInBuffer :: String -> Int
messageMarkerEndInBuffer xs = 14 + fst (head $ filter (allItemsDistinct. snd) $ zip [0..] (sliding 14 xs))

runPt2 = map messageMarkerEndInBuffer <$> readData