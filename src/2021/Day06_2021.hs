module Day06_2021
    (
    ) where

    import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1)
    import Data.Either.Combinators
    import Data.List(sort)

    parseFishes :: Parser [Int]
    parseFishes = do
        n <- sepBy1 (many1 digit) (char ',')
        return $ map read n

    readData :: IO String
    readData = readFile "resource/2021/day06_test"

    readAndParse :: IO [Int]
    readAndParse = fromRight' . parse parseFishes "" <$> readData
    ------------------

    nextDay :: [Int] -> [Int]
    nextDay [] = []
    nextDay (0 : xs) = 6 : 8 : nextDay xs
    nextDay (x : xs) = x - 1 : nextDay xs

    nTimes :: Int -> (a -> a) -> (a -> a)
    nTimes 0 _ = id
    nTimes 1 f = f
    nTimes n f = f . nTimes (n-1) f

    afterNDays n = nTimes n nextDay
    runPt1 = length . afterNDays 80
    ------------------------
    children n x = [m  |  m <- [0..n], (m - x - 1) `mod` 7 == 0]
    nextGeneration n x = if n - x < 9 then [] 
                         else [x + 9 + 7*m | m <- [0 .. (n - 9) `div` 7], x + 9 + 7*m <= n]

    gather :: Int -> (Int -> Int -> [Int]) -> [Int] -> [Int]
    gather n f = sort . concatMap (f n)

    accumulateNextGeneration :: Int -> [Int] -> [Int] -> [Int]
    accumulateNextGeneration n old [] = old
    accumulateNextGeneration n old new = accumulateNextGeneration n (old ++ new) (gather n nextGeneration new)

    run :: Int -> [Int] -> [Int]
    run n firstGen = accumulateNextGeneration n firstGen (gather n children firstGen)

    runPt1_redux = length . run 80
    runPt2 = length . run 256 
    -----------------------

    solution = runPt2 <$> readAndParse