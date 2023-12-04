module Day04_2023 (
) where

import Text.ParserCombinators.Parsec
    ( digit, letter, string, char, anyChar, parse, try, (<|>), Parser, many, many1, sepBy1, manyTill )
import Data.Either.Combinators
import Data.Maybe (mapMaybe)
import Data.List.Extra (trim)
import Data.Set (Set)
import qualified Data.Set as Set (fromList, intersection, size)

data Card = Card Int [Int] [Int] deriving Show

parseCardId :: Parser Int
parseCardId = do
    string "Card"
    n <- many1 (digit <|> char ' ')
    string ": "
    return $ read (trim n)

filterEmptyStrings = filter (not . null)

parseNumbers :: Parser ([Int], [Int])
parseNumbers = do
    ws <- sepBy1 (sepBy1 (many digit) (char ' ')) (string "| ")
    return ( map read $ filterEmptyStrings $ head ws, map read $ filterEmptyStrings $ ws !! 1)

parseCard :: Parser Card
parseCard = do
    id <- parseCardId
    (ws, ns) <- parseNumbers
    return  $ Card id ws ns 

------------------
readData = map (fromRight' . parse parseCard "") . lines <$> readFile "resource/2023/day04"

noOfWinningNumbers :: Card -> Int
noOfWinningNumbers (Card _ ws ns) = Set.size $ Set.intersection (Set.fromList ws) (Set.fromList ns)

score :: Card -> Int
score c | n == 0 = 0
        | otherwise = 2 ^ ( n - 1)
    where
        n = noOfWinningNumbers c

runPt1 = sum . map score <$> readData
