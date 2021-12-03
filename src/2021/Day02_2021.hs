module Day02_2021
    (
    ) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators

newtype Distance = X Int
newtype Depth = D Int
newtype Aim = A Int
data Operation = Up Int | Down Int | Forward Int
data Location = Position Distance Depth Aim

---------------
parseForward =  do
        string "forward "
        n <- many1 digit
        return $ Forward (read n)

parseDown =  do
        string "down "
        n <- many1 digit
        return $ Down (read n)

parseUp =  do
        string "up "
        n <- many1 digit
        return $ Up (read n)

parseOperation = parseForward <|> parseDown <|> parseUp
--------------

movePt1:: Location -> Operation -> Location
movePt1 (Position (X h) (D d) (A a)) (Up n) = Position (X (h - n)) (D d) (A a)
movePt1 (Position (X h) (D d) (A a)) (Down n) = Position (X (h + n)) (D d) (A a)
movePt1 (Position (X h) (D d) (A a)) (Forward n) = Position (X h)  (D (d + n)) (A a)

runCommands :: (Location -> Operation -> Location) -> [Operation] -> Location
runCommands move = foldl move (Position (X 0) (D 0) (A 0)) 

calculateValue :: Location -> Int
calculateValue (Position (X h) (D d) (A _)) = h * d


runPt1 :: [Operation] -> Int
runPt1 = calculateValue . runCommands movePt1

-----------------
movePt2:: Location -> Operation -> Location
movePt2 (Position (X h) (D d) (A a)) (Up n) = Position (X h) (D d) (A (a - n))
movePt2 (Position (X h) (D d) (A a)) (Down n) = Position (X h) (D d) (A (a + n))
movePt2 (Position (X h) (D d) (A a)) (Forward n) = Position (X (h + n))  (D (d + (a*n))) (A a)

runPt2 :: [Operation] -> Int
runPt2 = calculateValue . runCommands movePt2
------------------

main :: IO Int
main = runPt2. map (fromRight' . parse parseOperation "") . lines <$> readFile "resource/2021/day02"
