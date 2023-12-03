module Grid (
    get,
    neighbourhood,
    dimensions,
    Dimensions (..),
    neighbourhoodOnInfGrid
) where

import Models (Position(..))

data Dimensions = Dimensions Int Int deriving Show

get :: Position -> [[a]] -> a
get (Position x y) as = (as !! y) !! x

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
