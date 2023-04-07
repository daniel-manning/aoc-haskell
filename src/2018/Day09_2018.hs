module Day09_2018 (
    day09Pt1,
    day09Pt2
) where

import Control.Monad (join)
import Data.List (foldl')
import qualified Data.List.PointedList.Circular as PL
import Data.Maybe (fromJust)
import qualified Data.HashMap.Strict as HashMap


type Marbles = PL.PointedList Int

data GameStep = GameStep {
    score :: Int,
    marbles :: Marbles
} deriving Show

nextMarble :: Int -> Marbles -> GameStep
nextMarble marble ms | marble `mod` 23 == 0 = (\m -> GameStep (PL._focus m + marble) (fromJust $ PL.deleteRight m)) $ PL.moveN (-7) ms
                     | otherwise = GameStep 0 (PL.insertLeft marble $ PL.moveN 2 ms)

turns :: Int -> [Int]
turns n = join $ repeat (take n [1..])

alterOrUpdate :: Int -> Maybe Int -> Maybe Int
alterOrUpdate n (Just v) = Just (v+n)
alterOrUpdate n Nothing = Just n

type Scores = HashMap.HashMap Int Int

foldUp :: (Scores, Marbles) -> (Int, Int) -> (Scores, Marbles)
foldUp (scores, marbles) (marble, player) = (scores', ms')
    where
        GameStep score ms' = nextMarble marble marbles
        scores' = HashMap.alter (alterOrUpdate score) player scores

gameResult :: Int -> Int -> (Scores, Marbles)
gameResult noOfPlayers noOfMarbles = foldl' foldUp (HashMap.empty, fromJust $ PL.fromList [0]) $ zip [1..noOfMarbles] (turns noOfPlayers)

maxScore :: Int -> Int -> Int
maxScore noOfPlayers noOfMarbles = maximum $ HashMap.elems s
    where
        s = fst $ gameResult noOfPlayers noOfMarbles

day09Pt1 = maxScore 411 71058
day09Pt2 = maxScore 411 7105800