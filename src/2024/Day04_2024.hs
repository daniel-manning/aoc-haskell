module Day04_2024 (
) where

import ListUtils (convertToPositionList)
import Models (Position(..))
import Data.List (find, sort)
import Data.Maybe (catMaybes)

readData :: IO [[Char]]
readData = lines <$> readFile "resource/2024/day04"

createStarFromHead :: Int -> Position -> [[Position]]
createStarFromHead n p = map (take n . flip iterate p)[
    (\(Position m n) -> Position (m-1) (n - 1)),
    (\(Position m n) -> Position (m-1) n),
    (\(Position m n) -> Position (m-1) (n + 1)),
    (\(Position m n) -> Position m (n - 1)),
    (\(Position m n) -> Position m (n + 1)),
    (\(Position m n) -> Position (m+1) (n - 1)),
    (\(Position m n) -> Position (m+1) n),
    (\(Position m n) -> Position (m+1) (n + 1))
    ]

lookupChar :: [(Position, Char)] -> Position -> Maybe Char
lookupChar pcs p = fmap snd $ find (\l -> fst l == p) pcs

constructWord :: [(Position, Char)] -> [Position] -> String
constructWord pcs ps = catMaybes $ map (lookupChar pcs) ps

findHeadOfWord :: [(Position, Char)] -> [Position]
findHeadOfWord = map fst . filter (\l -> snd l == 'X')

--allStarWords :: [(Position, Char)] -> [[String]]
allStarWords pcs = concat $ map (map (constructWord pcs) . (createStarFromHead 4)) $ findHeadOfWord pcs

runPt1 = length . filter (== "XMAS") . allStarWords . convertToPositionList <$> readData
---------------------

createXFromMiddle :: Position -> [Position]
createXFromMiddle (Position x y) = [
    (Position (x-1) (y - 1)),
    (Position (x+1) (y + 1)),
    (Position (x-1) (y + 1)),
    (Position (x+1) (y - 1))
    ]

findMiddleOfWord :: [(Position, Char)] -> [Position]
findMiddleOfWord = map fst . filter (\l -> snd l == 'A')

allXWords pcs = map ((constructWord pcs) . createXFromMiddle) $ findMiddleOfWord pcs
runPt2 = length . filter (\s -> s == "MSMS" || s == "SMMS" || s == "MSSM" || s == "SMSM") . allXWords . convertToPositionList <$> readData
