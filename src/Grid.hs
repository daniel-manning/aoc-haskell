module Grid (
    get,
    neighbourhood,
    dimensions,
    Position (..),
    Dimensions (..)
) where

data Position = Position Int Int deriving (Eq, Ord, Show)
data Dimensions = Dimensions Int Int

get :: Position -> [[a]] -> a
get (Position x y) as = (as !! y) !! x

neighbourhood :: Dimensions -> Position -> [Position]
neighbourhood (Dimensions mx my) (Position x y) = filter (\(Position a b) -> (a>= 0) && (b >= 0) && (a < mx) && (b < my)) [Position x (y - 1), Position x (y + 1), Position (x - 1) y, Position  (x + 1) y]

dimensions :: [[a]] -> Dimensions
dimensions as = Dimensions (length (head as)) (length as)