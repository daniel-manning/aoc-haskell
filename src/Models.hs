module Models(
    Position(..)
    ) where

data Position = Position Int Int deriving (Ord, Eq, Show)