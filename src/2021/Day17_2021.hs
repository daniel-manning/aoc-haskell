module Day17_2021 where

import Text.ParserCombinators.Parsec (many1, letter, Parser, parse, char, string, (<|>), digit)
import Data.Either.Combinators ( fromRight' )
import Data.String.Utils (rstrip)
import Data.List (sort)

data Target = Target (Int, Int) (Int, Int) deriving Show

parseTarget:: Parser Target
parseTarget = do
    string "target area: x="
    x1 <- many1 (char '-' <|> digit)
    string ".."
    x2 <- many1 (char '-' <|> digit)
    string ", y="
    y1 <- many1 (char '-' <|> digit)
    string ".."
    y2 <- many1 (char '-' <|> digit)
    return $ Target (read x1, read x2) (read y1, read y2)

readData :: IO String
readData = rstrip <$> readFile "resource/2021/day17_test"
-------------------

readAndParse :: IO Target
readAndParse = fromRight' . parse parseTarget "" <$> readData


rootsOfTime :: Int -> (Int, Int) -> [Double]
rootsOfTime v (ymin, ymax) = sort $ filter (>= 0) $ r v =<< [ymin, ymax]

r :: Int -> Int -> [Double]
r v' y' = [(2*v + 1 + sqrt(4*v*v + 4*v + 1 - 8*y))/2, (2*v + 1 - sqrt(4*v*v + 4*v + 1 - 8*y))/2]
    where
        v = fromIntegral v'
        y = fromIntegral y'

run = rootsOfTime 0 . (\(Target _ y) -> y) <$> readAndParse