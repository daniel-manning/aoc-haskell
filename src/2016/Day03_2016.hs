module Day03_2016 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators (fromRight')
import Data.List
import Data.List.Extra (trim)
import Data.List.Split (chunksOf)

readData = lines <$> readFile "resource/2016/day03"

parseTriangle :: Parser [Int]
parseTriangle = map read <$> sepBy1 (many1 digit) (many1 (char ' '))

readAndParse :: IO [[Int]]
readAndParse = map ((fromRight' . parse parseTriangle "") . trim) <$> readData
------------
passesTriangleInequality :: [Int] -> Bool
passesTriangleInequality xs = k < sum (delete k xs)
    where
        k = maximum xs

runPt1 = length . filter passesTriangleInequality <$> readAndParse
-------------
turnColumnsAndChunk :: [[Int]] -> [[Int]]
turnColumnsAndChunk xs =   chunksOf 3 =<<  transpose xs


runPt2 =  length . filter passesTriangleInequality . turnColumnsAndChunk <$> readAndParse