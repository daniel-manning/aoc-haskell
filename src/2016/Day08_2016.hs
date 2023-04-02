{-# LANGUAGE DeriveGeneric #-}

module Day08_2016 (
) where

import GHC.Generics (Generic)
import Data.Hashable

import Text.ParserCombinators.Parsec
import Data.Either.Combinators

import Data.Maybe (fromJust)
import Data.List (foldl')
import qualified Data.HashSet as HashSet


data Instruction = Rect Int Int | RotateC Int Int | RotateR Int Int deriving (Show, Eq)
data Position = Position Int Int deriving (Show, Eq, Generic)
instance Hashable Position

parseRectangle :: Parser Instruction
parseRectangle = do
    string "rect "
    x <- many1 digit
    string "x"
    y <- many1 digit
    return $ Rect (read x) (read y)

parseColumnRotation :: Parser Instruction
parseColumnRotation = do
    string "rotate column x="
    x <- many1 digit
    string " by "
    y <- many1 digit
    return $ RotateC (read x) (read y)

parseRowRotation :: Parser Instruction
parseRowRotation = do
    string "rotate row y="
    x <- many1 digit
    string " by "
    y <- many1 digit
    return $ RotateR (read x) (read y)


parseInstruction :: Parser Instruction
parseInstruction = try parseRectangle <|> try parseColumnRotation <|> try parseRowRotation

readData :: IO [String]
readData = lines <$> readFile "resource/2016/day08"

readAndParse = map (fromRight' . parse parseInstruction "") <$> readData
----------------------------------
rectSize :: Instruction -> Maybe Int
rectSize (Rect a b) = Just (a*b)
rectSize _          = Nothing

countPixels :: [Instruction] -> Int
countPixels = sum . fromJust . sequence . filter (/= Nothing) . map rectSize

day08pt1 = countPixels <$> readAndParse
----------------------------------
--width = 7
--height = 3
width = 50
height = 6


runCommand :: HashSet.HashSet Position -> Instruction -> HashSet.HashSet Position
runCommand pos (Rect a b) = HashSet.union pos (HashSet.fromList [Position x y | x <- [0..(a-1)], y<- [0..(b-1)]])
runCommand pos (RotateC x n) = HashSet.map (\(Position a b) -> if a == x then Position a ((b + n) `mod` height) else Position a b) pos
runCommand pos (RotateR y n) = HashSet.map (\(Position a b) -> if b == y then Position ((a + n) `mod` width) b else Position a b) pos

foldAndDisplay :: [Instruction] -> IO ()
foldAndDisplay = showDisplay . foldl' runCommand HashSet.empty

showDisplay :: HashSet.HashSet Position -> IO ()
showDisplay ps = mapM_ putStrLn $ [[if Position x y `HashSet.member` ps then '*' else ' ' | x <- [0..(width-1)]] | y <- [0..(height-1)]]

day08pt2 = foldAndDisplay =<< readAndParse