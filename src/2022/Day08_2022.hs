module Day08_2022 (
) where

import Data.List
import ListUtils
import Data.Bifunctor

readData :: IO [String]
readData = lines <$> readFile "resource/2022/day08"

parseRow :: String -> [Int]
parseRow  = map (read . (: []))

readAndParse :: IO [[Int]]
readAndParse = map parseRow <$> readData
----------

largerThanElementsOnTheList :: (Ord a) => [(Position, a)] -> (Position, a) -> [(Position, a)]
largerThanElementsOnTheList xs x | maximum (map snd xs) < snd x = x: xs
                                 | otherwise = xs

treesThatCanBeSeenInRow:: [(Position, Int)] -> [(Position, Int)]
treesThatCanBeSeenInRow xs = nub (foldl' largerThanElementsOnTheList [head xs] (tail xs) ++ foldl' largerThanElementsOnTheList [head ys] (tail ys))
    where
        ys = reverse xs

boundingRectangle :: [(Position, Int)] -> (Int, Int)
boundingRectangle = (\xs -> (maximum $ map fst xs, maximum $ map snd xs)). map ((\(Position x y) -> (x, y)) . fst)

row :: [(Position, Int)] -> Int -> [(Position, Int)]
row ps n = sortBy (\(Position x1 y1, _)  (Position x2 y2, _)-> compare x1 x2) $ filter (\(Position x y, _) -> x == n) ps


column :: [(Position, Int)] -> Int -> [(Position, Int)]
column ps n = sortBy (\(Position x1 y1, _)  (Position x2 y2, _)-> compare y1 y2) $ filter (\(Position x y, _) -> y == n) ps


treeRowsAndColumns :: [(Position, Int)] -> [[(Position, Int)]]
treeRowsAndColumns pl = (\l -> map (row pl) [0..fst l]  ++ map (column pl) [0..snd l]) $ boundingRectangle pl

distinctTreesSeenInLines :: [[(Position, Int)]] -> [(Position, Int)]
distinctTreesSeenInLines ts = nub . treesThatCanBeSeenInRow =<< ts


runPt1 = length . nub . distinctTreesSeenInLines . treeRowsAndColumns . convertToPositionList <$> readAndParse
-----------

howFarCanISee :: Int -> [Int] -> Int
howFarCanISee ref hs = k + excess
    where
        k = length $ takeWhile (<ref) hs
        excess | length hs > k = 1
               | otherwise = 0

scoreTreeHousePosition :: Int -> [[Int]] -> Int
scoreTreeHousePosition ref = product . map (howFarCanISee ref)

treeHousePosition :: [(Position, Int)] -> Position -> Int
treeHousePosition ps (Position x y) = scoreTreeHousePosition ref [rb, ra, cb, ca]
    where
        r = map snd $ row ps x
        c = map snd $ column ps y
        ref = r !! y
        rb = reverse $ take y r
        ra = drop (y+1) r
        cb = reverse $ take x c
        ca = drop (x+1) c

scourForestForTreeHouses :: [(Position, Int)] -> [Int]
scourForestForTreeHouses ps = map (treeHousePosition ps) trees
    where
        b = boundingRectangle ps
        trees = [Position x y | x <- [0..fst b], y <- [0..snd b]]

runPt2 = maximum . scourForestForTreeHouses . convertToPositionList <$> readAndParse