module Grid (
    get,
    safeGet,
    neighbourhood,
    dimensions,
    Dimensions (..),
    neighbourhoodOnInfGrid,
    neighbourhoodOnInfGridWithoutD,
    gatherNeighbourValues,
    gatherNeighbourMaybeValues
) where

import Models (Position(..))
import Data.Maybe (isJust, fromJust)

data Dimensions = Dimensions Int Int deriving Show

get :: Position -> [[a]] -> a
get (Position x y) as = (as !! y) !! x

safeGet :: Position -> [[a]] -> Maybe a
safeGet (Position x y) as | x >= 0 && y >= 0 && x < dx && y <dy = Just $ (as !! y) !! x
                          | otherwise = Nothing
    where
        (Dimensions dx dy) = dimensions as

neighbourhood :: Dimensions -> Position -> [Position]
neighbourhood (Dimensions mx my) (Position x y) = filter (\(Position a b) -> (a>= 0) && (b >= 0) && (a < mx) && (b < my)) [Position x (y - 1), Position x (y + 1), Position (x - 1) y, Position  (x + 1) y]

dimensions :: [[a]] -> Dimensions
dimensions as = Dimensions (length (head as)) (length as)

--Includes diagonal
--  ***
--  * *
--  ***
neighbourhoodOnInfGrid :: Position -> [Position]
neighbourhoodOnInfGrid (Position x y) = [
    Position (x-1) (y-1), Position x (y-1), Position (x+1) (y-1),
    Position (x-1) y,                       Position (x+1) y,
    Position (x-1) (y+1), Position x (y+1), Position (x+1) (y+1)]

--Without diagonal
--   *
--  * *
--   *
neighbourhoodOnInfGridWithoutD :: Position -> [Position]
neighbourhoodOnInfGridWithoutD (Position x y) = [
                          Position x (y-1),
    Position (x-1) y,                       Position (x+1) y,
                          Position x (y+1)]

------
gatherNeighbourValues :: Position -> (Position -> [Position]) -> [[a]] -> [(Position, a)]
gatherNeighbourValues p fp as = map (\(a, b) -> (a, fromJust b)) $ filter (\(_,m) -> isJust m) $ map (\p -> (p, safeGet p as)) ps
    where
        ps = fp p

gatherNeighbourMaybeValues :: Position -> (Position -> [Position]) -> [[a]] -> [(Position, Maybe a)]
gatherNeighbourMaybeValues p fp as = map (\p -> (p, safeGet p as)) ps
    where
        ps = fp p