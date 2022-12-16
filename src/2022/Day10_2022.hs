module Day10_2022 (
) where

import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, string, char)
import Data.Either.Combinators (fromRight')
import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.List.Split (chunksOf)

data Instruction = AddX Int | NOOP deriving Show


parseAddX :: Parser Instruction
parseAddX = do
    string "addx "
    n <- many1 (char '-' <|> digit)
    return $ AddX (read n)

parseNOOP :: Parser Instruction
parseNOOP = do
    string "noop"
    return NOOP

readData :: IO [String]
readData = lines <$> readFile "resource/2022/day10_test_2"

parseInstruction = parseAddX <|> parseNOOP

readAndParse = map (fromRight' . parse parseInstruction "") <$> readData
---------------
cycles :: Instruction -> Int
cycles (AddX _) = 2
cycles NOOP = 1

data System = System {
    clock :: Int,
    value :: Int
} deriving (Show, Eq, Ord)

execute :: System -> Instruction -> System
execute (System c v) (AddX x) = System (c + cycles (AddX x)) (v + x)
execute (System c v) NOOP = System (c + cycles NOOP) v

runSystem :: System -> [Instruction] -> [System]
runSystem = scanl execute

signalStrengthAt :: [System] -> Int -> Int
signalStrengthAt ss n = (\(System _ v) -> v) $ last $ takeWhile (\(System c _) -> c < n) ss

--signalStrength :: [System] -> [Int]
signalStrength ss = map (\n -> n * signalStrengthAt ss n) $ takeWhile (<= cycleEnd) (20 : [60,100..])
    where
        cycleEnd = (\(System c _) -> c) $ last ss

runPt1 = sum . signalStrength . runSystem (System 0 1) <$> readAndParse
--------------

out = [
    "##..##..##..##..##..##..##..##..##..##..",
    "###...###...###...###...###...###...###.",
    "####....####....####....####....####....",
    "#####.....#####.....#####.....#####.....",
    "######......######......######......####",
    "#######.......#######.......#######....."]

printDisplay :: [String] -> IO ()
printDisplay = mapM_ putStrLn

printPixel :: Int -> Int -> Bool
printPixel c v = abs (c - v) < 2

cycleLines :: [[Int]]
cycleLines = chunksOf 40 [1..240]

modLines :: Int -> Int
modLines n | n `mod` 40 == 0  = 40
           | otherwise    = n `mod` 40 

cycleOutput :: [System] -> Int -> Char
cycleOutput ss c | printPixel (modLines c)  v = '#'
                 | otherwise                  = '.'
    where
        v = signalStrengthAt ss c

pixels :: [System] -> [String]
pixels ss = map (map (cycleOutput ss)) cycleLines


runPt2 = printDisplay =<< (pixels . runSystem (System 0 1) <$> readAndParse)
