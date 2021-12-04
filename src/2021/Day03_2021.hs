module Day03_2021
    (
    ) where


import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.List
import Data.Function (on)

data Binary = Zero | One deriving (Eq, Ord, Show)

------
parseZero =  do
        char '0'
        return Zero

parseOne =  do
        char '1'
        return One

parseDigit = parseZero <|> parseOne

parseBinary = many1 parseDigit
-----------
readData :: IO [String]
readData = lines <$> readFile "resource/2021/day03"

flipBinary :: Binary -> Binary
flipBinary Zero = One
flipBinary One = Zero

toDecimal :: [Binary] -> Int
toDecimal xs = sum $ zipWith exp xs (reverse [0 .. (length xs - 1)])
    where
        exp Zero _ = 0
        exp One n = 2^n

mostPopular :: Ord a => [a] -> a
mostPopular = head . maximumBy (compare `on` length).group.sort

gammaRate :: [[Binary]] -> [Binary]
gammaRate = map mostPopular . transpose

epsilonRate :: [[Binary]] -> [Binary]
epsilonRate = map flipBinary . gammaRate

powerConsumption :: [[Binary]] -> Int
powerConsumption xs = toDecimal gr * toDecimal er
    where
        gr = gammaRate xs
        er = epsilonRate xs 
---------------
filterByBitCriteria :: ([Binary] -> Binary) -> [[Binary]] -> [Binary]
filterByBitCriteria pred = filterByBitCriteria' pred []


filterByBitCriteria' :: ([Binary] -> Binary) -> [Binary] -> [[Binary]] -> [Binary]
filterByBitCriteria' pred prefix [x] = prefix ++ x
filterByBitCriteria' pred prefix xs =  filterByBitCriteria' pred (prefix ++ [d]) (map tail rs)
    where
        p x = x == pred (head $ transpose xs)
        rs = filter (p . head) xs
        d = pred (head $ transpose xs)

oxygenGeneratorRating = filterByBitCriteria mostPopular
co2ScrubberRating = filterByBitCriteria (flipBinary.mostPopular)
lifeSupportRating xs = toDecimal or * toDecimal cor
    where
        or = oxygenGeneratorRating xs
        cor = co2ScrubberRating xs

runPt1 = powerConsumption
runPt2 = lifeSupportRating

main :: IO Int
main = runPt2 . map (fromRight' . parse parseBinary "") <$> readData

--NB : This doesnt work in general as I havent told it what to do in tie-breaks