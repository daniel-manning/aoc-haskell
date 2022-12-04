module Day02_2016 (
) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1, string, (<|>))
import Data.Either.Combinators ( fromRight' )
import Data.List (foldl', scanl)

data Direction = U | D | L | R deriving Show
data Position = Position Int Int deriving (Eq, Ord, Show)

parseRight =  do
    char 'R'
    return R
parseLeft = do
    char 'L'
    return L
parseUp = do
    char 'U'
    return U
parseDown = do
    char 'D'
    return D

parseCommand :: Parser Direction
parseCommand = parseRight <|> parseLeft <|> parseUp <|> parseDown

parseList :: Parser [Direction]
parseList = many1 parseCommand

readData :: IO String
readData = readFile "resource/2016/day02"

readAndParse :: IO [[Direction]]
readAndParse = map (fromRight' . parse parseList "") . lines <$> readData
------------------
readFromPad :: Position -> Int
readFromPad (Position 0 2) = 1
readFromPad (Position 1 2) = 2
readFromPad (Position 2 2) = 3
readFromPad (Position 0 1) = 4
readFromPad (Position 1 1) = 5
readFromPad (Position 2 1) = 6
readFromPad (Position 0 0) = 7
readFromPad (Position 1 0) = 8
readFromPad (Position 2 0) = 9

canMoveOnGrid :: Int -> Int -> Direction -> Bool
canMoveOnGrid x y U = y < 2
canMoveOnGrid x y D = y > 0
canMoveOnGrid x y L = x > 0
canMoveOnGrid x y R = x < 2


moveDirection ::(Int -> Int -> Direction -> Bool) -> Position -> Direction -> Position
moveDirection g (Position x y) U | g x y U = Position x     (y+1)
                                 | otherwise = Position x y 
moveDirection g (Position x y) D | g x y D = Position x     (y-1)
                                 | otherwise = Position x y 
moveDirection g (Position x y) L | g x y L = Position (x-1)  y
                                 | otherwise = Position x y
moveDirection g (Position x y) R | g x y R = Position (x+1)  y
                                 | otherwise = Position x y 

moveFinger :: (Int -> Int -> Direction -> Bool) -> Position -> [Direction] -> Position
moveFinger g = foldl' (moveDirection g)

pressCode :: (Int -> Int -> Direction -> Bool) -> Position -> [[Direction]] -> [Position]
pressCode g p = tail . scanl (moveFinger g) p

runPt1 = map readFromPad . pressCode canMoveOnGrid (Position 1 1) <$> readAndParse
----------------------

readFromPad' :: Position -> Char
readFromPad' (Position 2 4) = '1'
readFromPad' (Position 1 3) = '2'
readFromPad' (Position 2 3) = '3'
readFromPad' (Position 3 3) = '4'
readFromPad' (Position 0 2) = '5'
readFromPad' (Position 1 2) = '6'
readFromPad' (Position 2 2) = '7'
readFromPad' (Position 3 2) = '8'
readFromPad' (Position 4 2) = '9'
readFromPad' (Position 1 1) = 'A'
readFromPad' (Position 2 1) = 'B'
readFromPad' (Position 3 1) = 'C'
readFromPad' (Position 2 0) = 'D'

canMoveOnGrid' :: Int -> Int -> Direction -> Bool
canMoveOnGrid' x y U = (x == 1  && y < 3) || (x == 2  && y < 4) || (x == 3  && y < 3)
canMoveOnGrid' x y D = (x == 1  && y > 1) || (x == 2  && y > 0) || (x == 3  && y > 1)
canMoveOnGrid' x y L = (y == 1  && x > 1) || (y == 2  && x > 0) || (y == 3  && x > 1)
canMoveOnGrid' x y R = (y == 1  && x < 3) || (y == 2  && x < 4) || (y == 3  && x < 3)

runPt2 = map readFromPad' . pressCode canMoveOnGrid' (Position 0 2) <$> readAndParse