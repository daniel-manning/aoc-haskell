module Day04_2022 (
) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, string)
import Data.Either.Combinators


data Assignment = Assignment Int Int deriving (Show, Eq)
type Section = (Assignment, Assignment)

parseSection :: Parser Section
parseSection = do
    xL <- many1 digit
    char '-'
    xH <- many1 digit
    char ','
    yL <- many1 digit
    char '-'
    yH <- many1 digit
    return (Assignment (read xL) (read xH), Assignment (read yL) (read yH))


readData :: IO [Section]
readData = map (fromRight' . parse parseSection "") . lines <$> readFile "resource/2022/day04"
-------------
assignmentIsSubset :: Section -> Bool
assignmentIsSubset (Assignment xL xH, Assignment yL yH) = (xL <= yL && xH >= yH) || (yL <= xL && yH >= xH)


runPt1 = length . filter assignmentIsSubset <$> readData
-------------
assignmentsOverlap :: Section -> Bool
assignmentsOverlap (Assignment xL xH, Assignment yL yH) =
    (xL <= yL && xH >= yH) ||
    (yL <= xL && yH >= xH) ||
    (yL <= xL && yH <= xH && xL <= yH) ||
    (xL <= yL && xH <= yH && yL <= xH)

runPt2 = length . filter assignmentsOverlap <$> readData