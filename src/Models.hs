module Models(
    Position(..),
    Velocity(..),
    Compass(..)
    ) where

data Position = Position Int Int deriving (Ord, Eq, Show)
data Velocity = Velocity Int Int deriving (Eq, Show)
data Compass = North | East | South | West deriving (Eq, Ord, Show)