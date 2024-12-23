module Day06_2024 (
    runPt1, runPt2
) where

import Data.Maybe (mapMaybe, fromJust)
import Models ( Position )
import ListUtils (Position(..), convertToPositionList)
import Data.List (find, sortBy, nub)
import ListUtils (groupBetweenBlankLines)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (snd3)
import qualified Data.Map.Strict as Map

data Compass = North | East | South | West deriving (Eq, Ord, Show)
data Guard = Guard Position Compass deriving (Eq, Show)
data Space = EMPTY | BLOCK | START deriving (Eq, Show)
data Patrol = ONGOING | LOOP | ENDED deriving (Eq, Show)
-----------
toSpace :: Char -> Maybe Space
toSpace '.' = Just EMPTY
toSpace '#' = Just BLOCK
toSpace '^' = Just START

turnRight :: Compass -> Compass
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

readData :: IO (Map.Map Position Space)
readData = Map.fromList . convertToPositionList . map (mapMaybe toSpace) . lines <$> readFile "resource/2024/day06"
----------------------
walk :: Position -> Compass -> Position
walk (Position x y) North = Position x      (y-1)
walk (Position x y) East =  Position (x+1)  y
walk (Position x y) South = Position x      (y+1)
walk (Position x y) West =  Position (x-1)  y

walkForward :: Map.Map Position Space -> (Guard, [(Position, Compass)], Patrol) -> (Guard, [(Position, Compass)], Patrol)
walkForward pss (Guard p c, ps, patrol) | s == Just EMPTY || s == Just START = (Guard np c, ps ++ [(np,c)], patrol)
                                        | s == Nothing = (Guard p c, ps, ENDED) --we've walked off the edge
                                        | otherwise = walkForward pss (Guard p (turnRight c), ps, patrol) --bumped so need to turn
    where
        np = walk p c 
        s = Map.lookup np pss

patrolUntilDone :: Map.Map Position Space -> (Guard, [(Position, Compass)], Patrol) -> (Guard, [(Position, Compass)], Patrol)
patrolUntilDone pss (Guard p c, ps, patrol) | patrol == ENDED = (Guard p c, ps, patrol)
                                            | otherwise = patrolUntilDone pss $ walkForward pss (Guard p c, ps, patrol)

guardStart :: Map.Map Position Space -> Position
guardStart = fst . fromJust . find (\(p, s) -> s == START) . Map.toList

countPath :: [(Position, Compass)] -> Int
countPath = Set.size . Set.fromList . map fst

runPt1 = countPath . snd3 . (\pss -> patrolUntilDone pss ((Guard (guardStart pss) North), [(guardStart pss, North)], ONGOING)) <$> readData 
----------------------

originalPatrol :: Map.Map Position Space -> [(Position, Compass)]
originalPatrol pss = snd3 $ patrolUntilDone pss ((Guard (guardStart pss) North), [(guardStart pss, North)], ONGOING)

walkForward' :: Map.Map Position Space -> (Guard, [(Position, Compass)], Patrol) -> (Guard, [(Position, Compass)], Patrol)
walkForward' pss (Guard p c, ps, patrol) | s == Nothing = (Guard p c, ps, ENDED) --we've walked off the edge
                                         | (np, c) `elem` ps = (Guard np c, ps, LOOP) -- if we're back where we've been we are LOOP
                                         | s == Just EMPTY || s == Just START = (Guard np c, (np,c):ps, patrol)
                                         | otherwise = walkForward' pss (Guard p (turnRight c), ps, patrol) --bumped so need to turn
    where
        np = walk p c 
        s = Map.lookup np pss

patrolUntilDoneOrLoop :: Map.Map Position Space -> (Guard, [(Position, Compass)], Patrol) -> Patrol
patrolUntilDoneOrLoop pss (Guard p c, ps, patrol) | patrol == ENDED || patrol == LOOP = patrol 
                                                  | otherwise = patrolUntilDoneOrLoop pss $ walkForward' pss (Guard p c, ps, patrol)

blockNextPosition :: Map.Map Position Space -> ([(Position, Compass)], [Position]) -> (Position, Compass) -> ([(Position, Compass)], [Position])
blockNextPosition mps (patrols, objects) (p,c) | p `elem` previousPositions = (patrols ++ [(p,c)], objects) --Can't place object on a position you've already walked through 
                                               | result == LOOP = (patrols ++ [(p,c)], objects ++ [p])
                                               | otherwise = (patrols ++ [(p,c)], objects)  
    where
        previousPositions = map fst patrols
        mps' = Map.insert p BLOCK mps 
        result = patrolUntilDoneOrLoop mps' ((uncurry Guard $ last patrols, init patrols, ONGOING))


allBlocksMakingLoops :: Map.Map Position Space -> [Position]
allBlocksMakingLoops mps = nub $ snd $ foldl (blockNextPosition mps) ([head patrol], []) (tail patrol)
    where
        patrol = originalPatrol mps

runPt2 = length . allBlocksMakingLoops <$> readData
