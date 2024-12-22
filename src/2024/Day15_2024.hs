module Day15_2024 () where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import ListUtils
import Data.Maybe
import Data.List (isInfixOf, any, find)
import Models (Position(..), Velocity(..), Compass(..))
import qualified Data.Map.Strict as Map

data Object = Box | Wall | Empty | Robot deriving (Eq, Show)
data Moves = Moves [Compass] deriving Show
-----------

parseObject :: Char -> Object
parseObject '#' = Wall
parseObject 'O' = Box
parseObject '@' = Robot
parseObject _ = Empty

parseMovement :: Char -> Compass
parseMovement '<' = West
parseMovement '>' = East
parseMovement 'v' = South
parseMovement '^' = North

readData :: IO ([[Object]], Moves)
readData = (\[os, ms] -> (reverse $ map (map parseObject) os, Moves $ map parseMovement (concat $ reverse ms))) . groupBetweenBlankLines . lines <$> readFile "resource/2024/day15"
---------
findRobot :: [(Position, Object)] -> Position
findRobot = fst . fromJust . find (\(_, o) -> o == Robot)

positionForDirection :: Position -> Compass -> Position
positionForDirection (Position x y) North = (Position x (y-1))
positionForDirection (Position x y) South = (Position x (y+1))
positionForDirection (Position x y) East = (Position (x+1) y)
positionForDirection (Position x y) West = (Position (x-1) y)

lookInDirection :: Map.Map Position Object -> Position -> Compass -> (Position, Object)
lookInDirection mpos p c = (p', fromJust $ Map.lookup p' mpos)
    where
        p' = positionForDirection p c

findChainOfMoves :: [(Position, Object)] -> Compass -> Map.Map Position Object -> Maybe [(Position, Object)]
findChainOfMoves (p:pos) c mpos | o == Wall = Nothing -- We've hit a wall and we can't move
                                | o == Empty = Just (p:pos) -- We've found an empty space and can move
                                | otherwise = findChainOfMoves ((p', o):p:pos) c mpos
    where
        (p', o) = lookInDirection mpos (fst p) c

updatePositions :: Map.Map Position Object -> [(Position, Object)] -> Compass -> Map.Map Position Object
updatePositions mpos pos c = foldl (\mpos' (p, o) -> Map.update (\_ -> Just Empty) p $ Map.update (\_ -> Just o) (positionForDirection p c) mpos') mpos pos


applyMove :: (Map.Map Position Object, Position) -> Compass -> (Map.Map Position Object, Position)
applyMove (mpos, p) c | isNothing pos = (mpos, p)
                      | otherwise  = (updatePositions mpos (fromJust pos) c, positionForDirection p c)
    where
        pos = findChainOfMoves [(p, Robot)] c mpos

runRobot :: ([[Object]], Moves) -> (Map.Map Position Object, Position)
runRobot (os, Moves ms) = foldl' applyMove (mpos, rp) ms
    where
        pos = convertToPositionList os
        rp = findRobot pos
        mpos = Map.fromList pos

onlyBoxes :: Map.Map Position Object -> [Position]
onlyBoxes = map fst . filter (\(_, o) -> o == Box) . Map.toList

gpsValue :: Position -> Int
gpsValue (Position x y) = x + 100*y

displayObject :: Object -> Char
displayObject Box = 'O'
displayObject Wall = '#'
displayObject Robot = '@'
displayObject Empty = '.'

generateStrings :: Map.Map Position Object -> [String]
generateStrings = map (map displayObject) . recreateGrid . Map.toList

printDisplay :: [String] -> IO ()
printDisplay = mapM_ putStrLn

view = generateStrings . fst . runRobot <$> readData

runPt1 = sum . map gpsValue . onlyBoxes . fst . runRobot <$> readData