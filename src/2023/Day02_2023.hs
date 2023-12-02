module Day02_2023
    (
    ) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import qualified Data.Set as Set (fromList, toList)
import Data.Maybe (mapMaybe)

newtype ID = ID Int deriving Show
data Bricks = Red Int | Blue Int | Green Int deriving (Eq, Ord, Show)
type Round = [Bricks]
data Bag = Bag { red :: Int, blue :: Int, green ::Int } deriving (Eq, Ord, Show)
-----------------
parseGameID =  do
        string "Game "
        n <- many1 digit
        string ": "
        return $ ID (read n)

parseGame :: Parser [Round]
parseGame = sepBy1 parseRound (string "; ")

parseRound :: Parser Round
parseRound = sepBy1 parseBrick (string ", ")

parseRed = do
        n <- many1 digit
        string " red"
        return $ Red (read n)

parseBlue = do
        n <- many1 digit
        string " blue"
        return $ Blue (read n)

parseGreen = do
        n <- many1 digit
        string " green"
        return $ Green (read n)

parseBrick = try parseRed <|> try parseBlue <|> try parseGreen

parseFullGame :: Parser (ID, [Round])
parseFullGame = do
    id <- parseGameID
    game <- parseGame
    return (id, game)
------------------
readData = map (fromRight' . parse parseFullGame "") . lines <$> readFile "resource/2023/day02_test"

maximumBag :: [Round] -> Bag
maximumBag rs = Bag {red = r, blue = b, green = g}
    where
        es = Set.toList $ Set.fromList $ concat rs
        g = maximum $ [ n | (Green n) <- es ]
        r = maximum $ [ n | (Red n) <- es ]
        b = maximum $ [ n | (Blue n) <- es ]

largestBag = Bag {red = 12, green = 13, blue = 14}

isGamePossible :: (ID, [Round]) -> Maybe ID
isGamePossible (id, game) | maximumBag game < largestBag = Just id
                          | otherwise = Nothing

runPt1 = sum . map (\(ID n) -> n) . mapMaybe isGamePossible <$> readData