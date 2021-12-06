module Day06_2021
    (
    ) where

    import Text.ParserCombinators.Parsec (many1, digit, Parser, parse, char, sepBy1)
    import Data.Either.Combinators
    import Data.List(sort, find, delete, group)
    import Data.Maybe(maybe)

    data State = State Int Int deriving (Eq, Show)

    parseFishes :: Parser [Int]
    parseFishes = do
        n <- sepBy1 (many1 digit) (char ',')
        return $ map read n

    readData :: IO String
    readData = readFile "resource/2021/day06"

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
    nextDayBucket :: [State] -> [State]
    nextDayBucket [] = []
    nextDayBucket ((State 0 n):s) = State 6 n:State 8 n:nextDayBucket s
    nextDayBucket ((State day num): s) = State (day-1) num : nextDayBucket s

    collectUpBuckets :: [State] -> [State]
    collectUpBuckets [] = []
    collectUpBuckets ((State d n):s) = f : collectUpBuckets s'
        where
            f =  maybe (State d n) (\(State _ b) -> State d (n + b)) $ find (\(State a b) -> a == d) s
            s' = maybe s (`delete` s) $ find (\(State a b) -> a == d) s


    groupDays :: [Int] -> [State]
    groupDays = map (\x -> State (head x) (length x)) . group . sort

    countUp = foldl (\c (State d n) -> c + n) 0

    runPt1_r = countUp . nTimes 80 (collectUpBuckets.nextDayBucket) . groupDays
    runPt2 = countUp . nTimes 256 (collectUpBuckets.nextDayBucket) . groupDays

    -----------------------

    solution = runPt2 <$> readAndParse