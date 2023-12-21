module Day11_2023
    (
    ) where

import Data.Maybe (mapMaybe)
import Models ( Position )
import ListUtils (Position(..), convertToPositionList)
import Data.Foldable (maximumBy)
import GHC.Conc (pseq)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, difference, toList)
import Data.Function (on)
import Data.Tuple.Extra (fst3)

data Space = Galaxy | Space deriving (Eq, Show)
newtype Width = Width Int deriving Show
newtype Height = Height Int deriving Show
newtype ExpansionFactor = ExpansionFactor Int deriving Show

toSpace :: Char -> Maybe Space
toSpace '.' = Just Space
toSpace '#' = Just Galaxy

filterWithSpaceInformation :: [(Position, Space)] -> ([(Position, Space)], Height, Width)
filterWithSpaceInformation ps = (filter (\(_, s) -> s == Galaxy) ps, Height rMax, Width cMax)
    where
        rMax = maximum $ map (\(Position x _, _) -> x) ps
        cMax = maximum $ map (\(Position _ y, _) -> y) ps


readData :: IO ([(Position, Space)], Height, Width)
readData = filterWithSpaceInformation . convertToPositionList . map (mapMaybe toSpace) . lines <$> readFile "resource/2023/day11"
------------------
rowsWithoutGalaxy :: ([(Position, Space)], Height, Width) -> [Int]
rowsWithoutGalaxy (ps, Height rMax, _) = Set.toList $ Set.fromList [0..rMax] `Set.difference` Set.fromList (map (\(Position _ y, _) -> y) ps)


columnsWithoutGalaxy :: ([(Position, Space)], Height, Width) -> [Int]
columnsWithoutGalaxy (ps, _, Width cMax) = Set.toList $ Set.fromList [0..cMax] `Set.difference` Set.fromList (map (\(Position x _, _) -> x) ps)

expandColumns :: ExpansionFactor -> [(Position, Space)] -> [Int] -> [(Position, Space)]
expandColumns _ ps [] = ps
expandColumns f@(ExpansionFactor ef) ps (n:ns) = expandColumns f (shiftGalaxyRow n f ps) (map (+ (ef-1)) ns)

shiftGalaxyRow :: Int -> ExpansionFactor -> [(Position, b)] -> [(Position, b)]
shiftGalaxyRow n (ExpansionFactor ef) = map (\(Position x y, s) -> if y > n then (Position x (y+(ef-1)), s) else (Position x y, s))

expandRows :: ExpansionFactor -> [(Position, Space)] -> [Int] -> [(Position, Space)]
expandRows _ ps [] = ps
expandRows f@(ExpansionFactor ef) ps (n:ns) = expandRows f (shiftGalaxyColumn n f ps) (map (+(ef-1)) ns)

shiftGalaxyColumn :: Int -> ExpansionFactor -> [(Position, b)] -> [(Position, b)]
shiftGalaxyColumn n (ExpansionFactor ef)= map (\(Position x y, s) -> if x > n then (Position (x+(ef-1)) y, s) else (Position x y, s))

expandSpace :: ExpansionFactor -> ([(Position, Space)], Height, Width) -> [(Position, Space)]
expandSpace f pshw = withColumns
    where
        rows = rowsWithoutGalaxy pshw
        columns = columnsWithoutGalaxy pshw
        ps = fst3 pshw
        withRows = expandRows f ps columns
        withColumns = expandColumns f withRows rows

minDistance :: Position -> Position -> Int
minDistance (Position x y) (Position x' y') = abs (x - x') + abs (y - y')

findAllDistances :: [Position] -> [Int]
findAllDistances [] = []
findAllDistances (p:ps) = map (minDistance p) ps ++ findAllDistances ps
runPt1 = sum . findAllDistances . map fst . expandSpace (ExpansionFactor 2) <$> readData
------------

runPt2 = sum . findAllDistances . map fst . expandSpace (ExpansionFactor 1000000) <$> readData
