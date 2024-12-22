module Day08_2024 (
) where

import Data.Maybe (mapMaybe, fromJust)
import Models ( Position )
import ListUtils (Position(..), convertToPositionList)
import Grid (dimensions, Dimensions (..))
import qualified Data.Map.Strict as Map
import Data.List (groupBy, nub, sortBy)
import Data.Ord (comparing)

data Antenna = Antenna Position Char deriving Show
data AntiNode = AntiNode Position deriving (Eq, Show)
--------
toAntennas :: (Position, Char) -> Maybe Antenna
toAntennas (_, '.') = Nothing
toAntennas (p, c) = Just $ Antenna p c

readData :: IO ([Antenna], Dimensions)
readData = (\ls -> ((mapMaybe toAntennas $ convertToPositionList ls), dimensions ls)) . lines <$> readFile "resource/2024/day08"

--TEST
--([Antenna (Position 3 1) '#',Antenna (Position 6 7) '#'],Dimensions 10 10)

--TEST2
--([Antenna (Position 3 1) '#',Antenna (Position 0 2) '#',Antenna (Position 2 6) '#',Antenna (Position 6 7) '#'],Dimensions 10 10)

--TEST3
--([Antenna (Position 6 0) '#',Antenna (Position 11 0) '#',Antenna (Position 3 1) '#',Antenna (Position 8 1) '0',Antenna (Position 4 2) '#',Antenna (Position 5 2) '0',Antenna (Position 10 2) '#',Antenna (Position 2 3) '#',
--Antenna (Position 7 3) '0',Antenna (Position 4 4) '0',Antenna (Position 9 4) '#',Antenna (Position 1 5) '#',Antenna (Position 6 5) 'A',Antenna (Position 3 6) '#',Antenna (Position 0 7) '#',Antenna (Position 7 7) '#',Antenna (Position 8 8) 'A',Antenna (Position 9 9) 'A',Antenna (Position 10 10) '#',Antenna (Position 10 11) '#'],Dimensions 12 12)

antiNodeForPair :: Antenna -> Antenna -> [AntiNode]
antiNodeForPair (Antenna (Position x1 y1) _) (Antenna (Position x2 y2) _) = [AntiNode (Position rx1 ry1), AntiNode (Position rx2 ry2)]
    where
        rx1 = x1 + x1 - x2
        ry1 = y1 + y1 - y2
        rx2 = x2 + x2 - x1
        ry2 = y2 + y2 - y1

makeMapOfAntenna :: [Antenna] -> Map.Map Char [Antenna]
makeMapOfAntenna = Map.fromList . map (\xs -> (fst $ head xs, map snd xs) ) . groupBy (\(c, _) (d, _) ->  c == d) . sortBy (comparing fst) . map (\(Antenna p c) -> (c, (Antenna p c)))

foldUpType :: [AntiNode] -> [Antenna] -> [AntiNode]
foldUpType ans [] = ans
foldUpType ans (a:as) = ((antiNodeForPair a) =<< as) ++ foldUpType ans as 

foldUpMap :: Dimensions -> Map.Map Char [Antenna] -> [AntiNode]
foldUpMap (Dimensions w h) = filter (\(AntiNode (Position x y)) -> x >= 0 &&  y >= 0 && x < w && y < h ) . Map.foldl foldUpType []

runPt1 = length . nub . (\(as, d) -> foldUpMap d $ makeMapOfAntenna as ) <$> readData
--------------
antiNodeForPair' :: Dimensions -> Antenna -> Antenna -> [AntiNode]
antiNodeForPair' (Dimensions w h) (Antenna (Position x1 y1) _) (Antenna (Position x2 y2) _) =
    [AntiNode (Position (x1 + k * (x1 - x2)) (y1 + k*(y1 - y2))) | k <- [0,1..maximumMultiplier], (x1 + k * (x1 - x2)) < w,  (y1 + k*(y1 - y2)) < h, (x1 + k * (x1 - x2)) >= 0,  (y1 + k*(y1 - y2)) >= 0] ++ [AntiNode (Position (x1 + k * (x1 - x2)) (y1 + k*(y1 - y2))) | k <- [(-1),(-2)..minimumMultiplier], (x1 + k * (x1 - x2)) < w,  (y1 + k*(y1 - y2)) < h, (x1 + k * (x1 - x2)) >= 0,  (y1 + k*(y1 - y2)) >= 0]
    where
        maximumMultiplier = maximum [w,h]
        minimumMultiplier = minimum [-w, -h]


foldUpType' :: Dimensions -> [AntiNode] -> [Antenna] -> [AntiNode]
foldUpType' d ans [] = ans
foldUpType' d ans (a:as) = ((antiNodeForPair' d a) =<< as) ++ foldUpType' d ans as 

foldUpMap' :: Dimensions -> Map.Map Char [Antenna] -> [AntiNode]
foldUpMap' d@(Dimensions w h) = filter (\(AntiNode (Position x y)) -> x >= 0 &&  y >= 0 && x < w && y < h ) . Map.foldl (foldUpType' d) []

runPt2 = length . nub . (\(as, d) -> foldUpMap' d $ makeMapOfAntenna as ) <$> readData