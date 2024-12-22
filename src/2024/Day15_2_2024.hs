module Day15_2_2024 ( view ) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import ListUtils
import Data.Maybe
import Data.List (isInfixOf, any, find)
import Data.Set (Set)
import qualified Data.Set as Set
import Models (Position(..), Velocity(..), Compass(..))
import qualified Data.Map.Strict as Map

data Object = BoxL | BoxR | Wall | Empty | Robot deriving (Ord, Eq, Show)
data Moves = Moves [Compass] deriving Show
-----------

parseObject :: Char -> [Object]
parseObject '#' = [Wall, Wall]
parseObject 'O' = [BoxL, BoxR]
parseObject '@' = [Robot, Empty]
parseObject _   = [Empty, Empty]

parseMovement :: Char -> Compass
parseMovement '<' = West
parseMovement '>' = East
parseMovement 'v' = South
parseMovement '^' = North

readData :: IO ([[Object]], Moves)
readData = (\[os, ms] -> (reverse $ map (concatMap parseObject) os, Moves $ map parseMovement (concat $ reverse ms))) . groupBetweenBlankLines . lines <$> readFile "resource/2024/day15"
---------
findRobot :: [(Position, Object)] -> Position
findRobot = fst . fromJust . find (\(_, o) -> o == Robot)

positionForDirection :: Compass -> Position -> Position
positionForDirection North (Position x y) = (Position x (y-1))
positionForDirection South (Position x y) = (Position x (y+1))
positionForDirection East (Position x y) = (Position (x+1) y)
positionForDirection West (Position x y) = (Position (x-1) y)

lookInDirection :: Map.Map Position Object -> Set Position -> Compass -> Set (Position, Object)
lookInDirection mpos p c = Set.map (\l -> (l, fromJust $ Map.lookup l mpos)) p'
    where
        p' = Set.map (positionForDirection c) p

------------------------------------ TODO - Need to expand the check as we push up or down not side to side
pullInBox :: (Position, Object) -> [(Position, Object)]
pullInBox (Position x y, BoxL) = [(Position x y, BoxL), (Position (x+1) y, BoxR)]
pullInBox (Position x y, BoxR) = [(Position (x-1) y, BoxL), (Position x y, BoxR)]
pullInBox (Position x y, o) = [(Position x y, o)]

addMissingBoxes :: Set (Position, Object) ->  Set (Position, Object)
addMissingBoxes spos = Set.fromList $ pullInBox =<< Set.toList spos --Assume boxes not broken up


findChainOfMoves :: [Set (Position, Object)] -> Compass -> Map.Map Position Object -> Maybe [(Position, Object)]
findChainOfMoves (p:pos) c mpos | any (\(_,o) -> o == Wall) sp = Nothing -- We've hit a wall and we can't move
                                | all (\(_,o) -> o == Empty) sp = Just $ concat (map Set.toList (p':pos)) -- We've found empty spaces and can move
                                | otherwise = findChainOfMoves (p''':p':pos) c mpos
    where
        p' = if c == North || c == South then addMissingBoxes p else p
        p'' = lookInDirection mpos (Set.map fst p') c
        sp = Set.toList p''
        p''' = Set.filter (\(p, o) -> o == BoxL || o == BoxR) p'' --Reduce the look forward to just the boxes ahead

updatePositions :: Map.Map Position Object -> [(Position, Object)] -> Compass -> Map.Map Position Object
updatePositions mpos pos c = foldl (\mpos' (p, o) -> Map.update (\_ -> Just Empty) p $ Map.update (\_ -> Just o) (positionForDirection c p) mpos') mpos pos


applyMove :: (Map.Map Position Object, Position) -> Compass -> (Map.Map Position Object, Position)
applyMove (mpos, p) c | isNothing pos = (mpos, p)
                      | otherwise  = (updatePositions mpos (fromJust pos) c, positionForDirection c p)
    where
        pos = findChainOfMoves [Set.fromList [(p, Robot)]] c mpos


runRobot :: ([[Object]], Moves) -> (Map.Map Position Object, Position)
runRobot (os, Moves ms) = foldl' applyMove (mpos, rp) ms
    where
        pos = convertToPositionList os
        rp = findRobot pos
        mpos = Map.fromList pos

runRobotWithHistory :: ([[Object]], Moves) -> [(Map.Map Position Object, Position)]
runRobotWithHistory (os, Moves ms) = foldl' (\a b -> (applyMove (head a) b) : a) [(mpos, rp)] ms
    where
        pos = convertToPositionList os
        rp = findRobot pos
        mpos = Map.fromList pos

--------------------------

onlyBoxes :: Map.Map Position Object -> [Position]
onlyBoxes = map fst . filter (\(_, o) -> o == BoxL) . Map.toList

gpsValue :: Position -> Int
gpsValue (Position x y) = x + 100*y

displayObject :: Object -> Char
displayObject BoxL = '['
displayObject BoxR = ']'
displayObject Wall = '#'
displayObject Robot = '@'
displayObject Empty = '.'

generateStrings :: Map.Map Position Object -> [String]
generateStrings = map (map displayObject) . recreateGrid . Map.toList

printDisplay :: [String] -> IO ()
printDisplay = mapM_ putStrLn

view :: IO [String]
view = concat . map (generateStrings . fst) . runRobotWithHistory <$> readData
showMe = mapM_ putStrLn =<< view

runPt2 = sum . map gpsValue . onlyBoxes . fst . runRobot <$> readData
