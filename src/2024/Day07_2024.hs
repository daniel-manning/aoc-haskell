module Day07_2024 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.List.NonEmpty
import Prelude hiding ((||))

data Equation = Equation Int (NonEmpty Int) deriving Show
--------
parseEquation :: Parser Equation
parseEquation = do
    x <- many1 digit
    string ": "
    y <- sepBy1 (many1 digit) (space)
    return $ Equation (read x) (map read y)

readData :: IO [Equation]
readData = map (fromRight' . parse parseEquation "" ) . lines <$> readFile "resource/2024/day07"

evaluateOperators :: [Int] -> Int -> [Int]
evaluateOperators ns x = (\n -> [n + x, n * x]) =<< ns

findPossibleValues :: Equation -> [Int]
findPossibleValues (Equation n xs) = foldl evaluateOperators [head xs] (tail xs)

canMakeTarget :: Equation -> Bool
canMakeTarget eq@(Equation n _) = n `elem` (findPossibleValues eq)

testValue :: Equation -> Int
testValue (Equation n _) = n

runPt1 = sum . map testValue . filter canMakeTarget <$> readData

----------------

(||) :: Int -> Int -> Int
(||) a b = read $ show a ++ show b

evaluateOperators' :: Int -> [Int] -> Int -> [Int]
evaluateOperators' maximum ns x = filter (<= maximum) $ (\n -> [n + x, n * x, n || x]) =<< ns

findPossibleValues' :: Equation -> [Int]
findPossibleValues' (Equation n xs) = foldl (evaluateOperators' n) [head xs] (tail xs)

canMakeTarget' :: Equation -> Bool
canMakeTarget' eq@(Equation n _) = n `elem` (findPossibleValues' eq)

runPt2 = sum . map testValue . filter canMakeTarget' <$> readData