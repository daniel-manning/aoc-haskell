module Day12_2023 (
) where

import Data.Maybe (mapMaybe)
import Data.String.Utils (split)
import Data.List (group)
import Data.Bifunctor (bimap)

data Spring = Damaged | Operational | Unknown deriving (Eq, Show)

toSpring :: Char -> Maybe Spring
toSpring '#' = Just Damaged
toSpring '.' = Just Operational
toSpring '?' = Just Unknown
toSpring _ = Nothing

readLine :: String -> ([Spring], [Int])
readLine s = (t, l)
    where
        ws = words s
        t = mapMaybe toSpring (head ws)
        l = map read $ split "," (ws !! 1)

readData :: IO [([Spring], [Int])]
readData = map readLine . lines <$> readFile "resource/2023/day12"
-------------------------------
trimDown :: [Spring] -> [Spring]
trimDown = reverse . dropWhile (/= Operational) . reverse . takeWhile (/= Unknown)

defectList :: [Spring] -> [Int]
defectList = map snd . filter (\(s, _) -> s == Damaged) . map (\l -> (head l, length l)) . group

reduce :: ([Spring], [Int]) -> ([Spring], [Int])
reduce (ss, ns) = (drop (length ss') ss, drop (length dl) ns)
    where
        ss' = trimDown ss
        dl = defectList ss'

reduceFirstBlock :: ([Spring], [Int]) -> ([Spring], [Int])
reduceFirstBlock (ss, ns) | head ss == Unknown  && ds == head ns  = (drop (ds + 2) ss, drop 1 ns)
                          | otherwise = (ss, ns)
    where
        ds = length $ takeWhile (== Damaged) (drop 1 ss)

cutDownExample :: ([Spring], [Int]) -> ([Spring], [Int])
cutDownExample (ss, ns) = (ss'''', ns'''')
    where
        (ss', ns') = reduce (ss, ns)
        (ss'', ns'') = bimap reverse reverse $ reduce (reverse ss', reverse ns')
        (ss''', ns''') = reduceFirstBlock (ss'', ns'')
        (ss'''', ns'''') = bimap reverse reverse $ reduceFirstBlock (reverse ss''', reverse ns''')

generateUnknowns :: Int -> [[Spring]]
generateUnknowns 0 = [[]]
generateUnknowns n =  map (Damaged :) k ++ map (Operational:) k
    where
        k = generateUnknowns (n -1)

foldInUnknowns :: [Spring] -> [Spring] -> [Spring]
foldInUnknowns xs [] = xs
foldInUnknowns [] _ = []
foldInUnknowns (Unknown: xs) (u:us) = u: foldInUnknowns xs us
foldInUnknowns (x: xs) (u:us) = x: foldInUnknowns xs (u:us)

passesGrouping :: [Int] -> [Spring] -> Bool
passesGrouping ns ss = defectList ss == ns

countPossibilities :: ([Spring], [Int]) -> Int
countPossibilities (ss, ns) = length $ filter (passesGrouping ns) samples
    where
        n = length $ filter (== Unknown) ss
        unknowns = generateUnknowns n
        samples = map (foldInUnknowns ss) unknowns

runPt1 = sum . map (countPossibilities . cutDownExample) <$> readData --HORRIBLE HORRIBLE performance, needs more intelligent filtering upfront
-------------------

