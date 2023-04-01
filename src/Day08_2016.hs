module Day08_2016 (
) where


import Text.ParserCombinators.Parsec
import Data.Either.Combinators

import Data.Maybe (fromJust)


data Instruction = Rect Int Int | RotateC Int Int | RotateR Int Int deriving (Show, Eq)


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