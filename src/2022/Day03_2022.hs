module Day03_2022 (
) where

import qualified Data.Set as S
import Data.Char (ord, isLower)
import Data.List (foldl1')
import Data.List.Split (chunksOf)

type RuckSack = String

readData ::IO [RuckSack]
readData =  lines <$> readFile "resource/2022/day03"

divideCompartments:: RuckSack -> (String, String)
divideCompartments rs = splitAt h rs
    where
        h = length rs `div` 2

findSharedElement :: (String, String) -> Char
findSharedElement (as, bs) =  head $ S.toList $ S.fromList as `S.intersection` S.fromList bs

priorityScore :: Char -> Int
priorityScore c | isLower c = ord c - 96
                | otherwise = ord c - 38

runPt1 = sum . map (priorityScore . findSharedElement . divideCompartments) <$> readData
--------------
divideInToGroups :: [RuckSack] -> [[RuckSack]]
divideInToGroups = chunksOf 3

findBadge :: [RuckSack] -> Char
findBadge rs = head $ S.toList $ foldl1' S.intersection $ map S.fromList rs

runPt2 = sum . map (priorityScore . findBadge) . divideInToGroups <$> readData