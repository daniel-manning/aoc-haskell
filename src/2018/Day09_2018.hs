module Day09_2018 (
    day09Pt1
) where

import Control.Monad (join)
import Data.List (foldl')
import qualified Data.HashMap.Strict as HashMap

insertAtPos :: Int -> a -> [a] -> [a]
insertAtPos n v vs = as ++ [v] ++ bs
    where
        (as, bs) = splitAt n vs

removeAtPos ::Int -> [a] -> (a, [a])
removeAtPos n as = (as !! n, take n as ++ drop (n+1) as)

advance :: Int -> Int -> [Int] -> Int
advance n p ms = (p + n) `mod` (length ms)

data GameStep = GameStep {
    score :: Int,
    currentPos :: Int,
    marbles :: [Int]
} deriving Show

nextMarble :: Int -> Int -> [Int] -> GameStep
nextMarble currentPos marble ms | marble `mod` 23 == 0 = GameStep (marble + r) b ms'
                                | otherwise = GameStep 0 k (insertAtPos k marble ms)
    where
        k = advance 2 currentPos ms
        b = advance (-7) currentPos ms
        (r, ms') = removeAtPos b ms

turns :: Int -> [Int]
turns n = join $ repeat (take n [1..])

alterOrUpdate :: Int -> Maybe Int -> Maybe Int
alterOrUpdate n (Just v) = Just (v+n)
alterOrUpdate n Nothing = Just n

type Scores = HashMap.HashMap Int Int

foldUp :: (Scores, [Int], Int) -> (Int, Int) -> (Scores, [Int], Int)
foldUp (scores, marbles, currentPos) (marble, player) = (scores', ms', pos')
    where
        GameStep score pos' ms' = nextMarble currentPos marble marbles
        scores' = HashMap.alter (alterOrUpdate score) player scores

gameResult :: Int -> Int -> (Scores, [Int], Int)
gameResult noOfPlayers noOfMarbles = foldl' foldUp (HashMap.empty, [0], 0) $ zip [1..noOfMarbles] (turns noOfPlayers)

maxScore :: Int -> Int -> Int
maxScore noOfPlayers noOfMarbles = maximum $ HashMap.elems s
    where
        (s, _, _) = gameResult noOfPlayers noOfMarbles

day09Pt1 = maxScore 411 71058