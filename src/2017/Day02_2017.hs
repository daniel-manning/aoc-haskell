{-# LANGUAGE TupleSections #-}
module Day02_2017 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators (fromRight')

readData = lines <$> readFile "resource/2017/day02"

parseItems :: Parser [Int]
parseItems = map read <$> sepBy1 (many1 digit) (char ' ' <|> char '\t') --TODO: tab character literal

readAndParse :: IO [[Int]]
readAndParse = map (fromRight' . parse parseItems "") <$> readData
-----------------
check xs = maximum xs - minimum xs 

runPt1 = sum . map check <$> readAndParse
------------------
evenlyDivisible :: Int -> Int -> Bool
evenlyDivisible a b = minimum [a,b] == gcd a b 

cross :: [a] -> [(a, a)]
cross [] = []
cross (x : xs) = map (x,) xs ++ cross xs

onlyDivisblePairs :: [Int] -> (Int, Int)
onlyDivisblePairs = head . filter (uncurry evenlyDivisible) . cross

dividePairEvenly :: (Int, Int) -> Int
dividePairEvenly (a, b) = maximum [a, b] `div` minimum [a, b]

runPt2 = sum . map (dividePairEvenly . onlyDivisblePairs) <$> readAndParse