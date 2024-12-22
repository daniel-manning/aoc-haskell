module Day10_2024 (
) where

import Models(Position(..))
import Grid (gatherNeighbourValues, neighbourhoodOnInfGridWithoutD)
import ListUtils (convertToPositionList)
import Data.Ord (comparing)
import Data.List (groupBy, nub, sortBy)

readData :: IO [[Int]]
readData = map (map (read . (\c -> [c]))) . lines <$> readFile "resource/2024/day10"
--------------
findTrailHeads :: [[Int]] -> [(Position, Int)]
findTrailHeads = filter (\(_,a) -> a == 0) . convertToPositionList

nextStepStrictlyIncreasing :: [[Int]] -> (Position, Int) -> [(Position, Int)]
nextStepStrictlyIncreasing as (p, n) = filter (\(_, m) -> m == n + 1) $ gatherNeighbourValues p neighbourhoodOnInfGridWithoutD as

pathsUp :: [[Int]] -> [(Position, Int)] -> [[(Position, Int)]]
pathsUp as ((p, n): ps) = map (\l -> (l:(p,n):ps)) $ nextStepStrictlyIncreasing as (p, n)

walkOn as ps = (pathsUp as) =<< ps

fullWalks :: [[Int]] -> [(Position, Int)] -> [[(Position, Int)]]
fullWalks as ps = (iterate (walkOn as) (map (\p -> [p]) ps)) !! 9

score :: [[(Position, Int)]] -> [(Position, Int)]
score ps = map (\xs -> (fst $ head xs, length xs)) $ groupBy (\m n -> fst m == fst n) $ sortBy (comparing fst) $ nub $ map (\p -> (fst $ last p, fst $ head p)) ps

runPt1 = sum . map snd . score . (\as -> fullWalks as (findTrailHeads as)) <$> readData
-------------
score' :: [[(Position, Int)]] -> [(Position, Int)]
score' ps = map (\xs -> (fst $ head xs, length xs)) $ groupBy (\m n -> fst m == fst n) $ sortBy (comparing fst) $ map (\p -> (fst $ last p, fst $ head p)) ps

runPt2 = sum . map snd . score' . (\as -> fullWalks as (findTrailHeads as)) <$> readData
