module Day12_2024 (
) where

import Models (Position(..))
import Grid
import ListUtils (convertToPositionList)
import Data.Set (Set)
import qualified Data.Set as Set

data Region = Region { plot :: Char, areas :: Set Position, perimeter :: Int} deriving Show

readData :: IO [[Char]]
readData = lines <$> readFile "resource/2024/day12_test"
----------
createNewRegion :: [[Char]] -> Position -> Region
createNewRegion cs p = Region { plot = c, areas = as, perimeter = n }
    where
        c = get p cs
        as = allPlotsForStart cs p c
        n = countPerimeters cs as c

countPerimeters :: [[Char]] -> Set Position -> Char -> Int
countPerimeters cs sps c = sum $ map ns (Set.toList sps)
    where
      ns p = length $ filter (\(_, b) -> b /= Just c) $ gatherNeighbourMaybeValues p neighbourhoodOnInfGridWithoutD cs

gatherNextPlots :: [[Char]] -> Position -> Char -> [Position]
gatherNextPlots cs p c = map fst $ filter (\(p', c') -> c' == c) $ gatherNeighbourValues p neighbourhoodOnInfGridWithoutD cs

newPositions :: [[Char]] -> Set Position -> Char -> Position -> Set Position
newPositions cs ops c p = Set.difference ns ops
    where
        ns = Set.fromList $ gatherNextPlots cs p c

findNextPlots :: [[Char]] -> Set Position -> Set Position -> Char -> Set Position
findNextPlots cs ops ps c = foldl Set.union Set.empty $ map (newPositions cs ops c) (Set.toList ps)

walkToBoundary :: [[Char]] -> Set Position -> Set Position -> Char -> Set Position
walkToBoundary cs ops nps c | null ns = ops'
                            | otherwise = walkToBoundary cs ops' ns c
    where
        ns = findNextPlots cs ops nps c
        ops' = Set.union ops nps

allPlotsForStart :: [[Char]] -> Position -> Char -> Set Position
allPlotsForStart cs p c = walkToBoundary cs Set.empty (Set.fromList [p]) c

carveUpRegions :: [[Char]] -> [Region]
carveUpRegions cs = carveUpRegions' cs (map fst $ convertToPositionList cs) []
    where
        carveUpRegions' cs ps rs | null ps = rs
                             | otherwise =  carveUpRegions' cs ps' (r:rs)
            where
                r = createNewRegion cs (head ps)
                ps' = Set.toList (Set.difference (Set.fromList ps) (areas r))

price :: Region -> Int
price r = (length $ areas r) * (perimeter r)

runPt1 = sum . map price . carveUpRegions <$> readData
-----------

createNewRegion' :: [[Char]] -> Position -> Region
createNewRegion' cs p = Region { plot = c, areas = as, perimeter = n }
    where
        c = get p cs
        as = allPlotsForStart cs p c
        n = 0 --countSides cs as c

--countSides :: [[Char]] -> Set Position -> Char -> Int
countSides cs sps c = ns'
    where
      ns p = filter (\(_, b) -> b /= Just c) $ gatherNeighbourMaybeValues p neighbourhoodOnInfGridWithoutD cs
      ns' = concat $ map ((map fst). ns) (Set.toList sps)

pullEdge :: [Postion] -> Position -> [Position]
pullEdge ps p = _
    where
        

carveUpRegions' :: [[Char]] -> [Region]
carveUpRegions' cs = carveUpRegions'' cs (map fst $ convertToPositionList cs) []
    where
        carveUpRegions'' cs ps rs | null ps = rs
                                  | otherwise =  carveUpRegions'' cs ps' (r:rs)
            where
                r = createNewRegion' cs (head ps)
                ps' = Set.toList (Set.difference (Set.fromList ps) (areas r))

test = ["AAAA","BBCD","BBCC","EEEC"]

runPt2 = sum . map price . carveUpRegions' <$> readData