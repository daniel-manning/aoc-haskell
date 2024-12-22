module Day14_2024 (
    runPt2, runPt2'
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import ListUtils
import Data.Maybe
import Data.List (isInfixOf, any)
import Models (Position(..), Velocity(..))

data Robot = Robot Position Velocity deriving Show
data Room = Room Int Int deriving Show
-----------
parseRobot :: Parser Robot
parseRobot = do
    string "p="
    x <- many1 digit
    string ","
    y <- many1 digit
    string " v="
    vx <- many1 (char '-' <|> digit)
    string ","
    vy <- many1 (char '-' <|> digit)
    return $ Robot (Position (read x) (read y)) (Velocity (read vx) (read vy))

--room = Room 11 7
room = Room 101 103

readData :: IO [Robot]
readData = map (fromRight' . parse parseRobot "") . lines <$> readFile "resource/2024/day14"
---------
advance :: Room -> Robot -> Robot
advance (Room mx my) (Robot (Position x y) (Velocity vx vy)) = Robot (Position x' y') (Velocity vx vy)
    where
        x' = (x + vx) `mod` mx
        y' = (y + vy) `mod` my

allAdvance = map (advance room)

inNSteps n rs = (iterate allAdvance rs) !! n

countUpRegions (Room rx ry) rs = [ 
    length $ filter (\(Robot (Position x y) _) -> x < rx `div` 2 && y < ry `div` 2) rs,
    length $ filter (\(Robot (Position x y) _) -> x < rx `div` 2 && y > ry `div` 2) rs,
    length $ filter (\(Robot (Position x y) _) -> x > rx `div` 2 && y < ry `div` 2) rs,
    length $ filter (\(Robot (Position x y) _) -> x > rx `div` 2 && y > ry `div` 2) rs
    ]

runPt1 = product . (countUpRegions room) . inNSteps 100 <$> readData
---------------------
generateGrid :: Room -> [Robot] -> [String]
generateGrid (Room mx my) rs = recreateGrid [(Position x y, if (Position x y) `elem` ps then '.' else ' ') | x <- [0..(mx-1)], y <- [0..(my-1)]]
    where
        ps = map (\(Robot p _) -> p) rs

printDisplay :: [String] -> IO ()
printDisplay = mapM_ putStrLn

interesting = [16,119..]

runPt2 n = printDisplay =<< generateGrid room . inNSteps (interesting !! n) <$> readData

hasLineOfPoints :: [String] -> Bool
hasLineOfPoints ss = any ("........" `isInfixOf`) ss

makeRobotString :: [Robot] -> Int -> (Int, [String])
makeRobotString rs l = (l, generateGrid room $ inNSteps (interesting !! l) rs)

runPt2' = take 1 . (\rs -> filter (\(a,b) -> hasLineOfPoints b) $  map (makeRobotString rs) [1..]) <$> readData

test = interesting !! 78