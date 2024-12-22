module Day13_2024 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import ListUtils
import Data.Maybe

data ButtonA = ButtonA Integer Integer deriving Show
data ButtonB = ButtonB Integer Integer deriving Show
data Prize = Prize Integer Integer deriving Show
data ClawMachine = ClawMachine ButtonA ButtonB Prize deriving Show
-----------
parseButtonA :: Parser ButtonA
parseButtonA = do
    string "Button A: X+"
    x <- many1 digit
    string ", Y+"
    y <- many1 digit
    return $ ButtonA (read x) (read y)

parseButtonB :: Parser ButtonB
parseButtonB = do
    string "Button B: X+"
    x <- many1 digit
    string ", Y+"
    y <- many1 digit
    return $ ButtonB (read x) (read y)

parsePrize :: Parser Prize
parsePrize = do
    string "Prize: X="
    x <- many1 digit
    string ", Y="
    y <- many1 digit
    return $ Prize (read x) (read y)

parseMachine :: [String] -> ClawMachine
parseMachine [x, y, z] = ClawMachine
    (fromRight' $ parse parseButtonA "" x)
    (fromRight' $ parse parseButtonB "" y) 
    (fromRight' $ parse parsePrize "" z)

readData :: IO [ClawMachine]
readData = map (parseMachine . reverse) . groupBetweenBlankLines . lines <$> readFile "resource/2024/day13"

------------
solution :: ClawMachine -> Maybe (Integer, Integer)
solution (ClawMachine (ButtonA a c) (ButtonB b d) (Prize x y)) | det /= 0 && bothDividedByDet = Just ((d*x - b*y) `div` det, (a*y - c*x) `div` det)
                                                               | otherwise = Nothing
    where
        det = a*d - b*c
        bothDividedByDet = (d*x - b*y) `mod` det == 0 && (a*y - c*x) `mod` det == 0

removeBadSolutions Nothing = Nothing
removeBadSolutions (Just (x, y)) | x >= 0 && x <= 100 && y >= 0 && y <= 100 = Just (x, y)
                                 | otherwise = Nothing

runPt1 = sum . map (\(a,b) -> 3*a + b) . catMaybes . map (removeBadSolutions . solution) <$> readData
-----------

adjustPrize :: ClawMachine -> ClawMachine
adjustPrize (ClawMachine a b (Prize x y)) = ClawMachine a b (Prize (x + 10000000000000) (y + 10000000000000))

removeBadSolutions' Nothing = Nothing
removeBadSolutions' (Just (x, y)) | x >= 0 && y >= 0 = Just (x, y)
                                  | otherwise = Nothing

runPt2 = sum . map (\(a,b) -> 3*a + b) . catMaybes . map (removeBadSolutions' . solution . adjustPrize) <$> readData