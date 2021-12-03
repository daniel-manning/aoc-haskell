module Day05_2020
    (
    ) where

    import Data.Maybe (fromJust)
    import Data.List (maximum, sort)
    import Data.Set (Set)
    import qualified Data.Set as Set

    data Range = Range Int Int deriving Show
    data Section = F | B | L | R deriving Show
    data Seat = Seat Int Int deriving Show

    binaryPartition :: Range -> Section -> Range
    binaryPartition (Range l h) F = Range l (l + (h - l) `div` 2)
    binaryPartition (Range l h) L = Range l (l + (h - l) `div` 2)
    binaryPartition (Range l h) B = Range (l + (h - l + 1) `div` 2) h
    binaryPartition (Range l h) R = Range (l + (h - l + 1) `div` 2) h

    sortUntilConvergence :: Range -> [Section] -> Maybe Int
    sortUntilConvergence (Range l h) [] | l == h = Just l
                                        | otherwise = Nothing
    sortUntilConvergence r (c:cs) = sortUntilConvergence (binaryPartition r c) cs

    toSection :: Char -> Section
    toSection 'F' = F
    toSection 'B' = B
    toSection 'L' = L
    toSection 'R' = R

    findRow :: String -> Maybe Int
    findRow input = sortUntilConvergence (Range 0 127) (map toSection input)
    findColumn input = sortUntilConvergence (Range 0 7) (map toSection input)

    findSeat :: String -> Maybe Seat
    findSeat input = (Seat <$> findRow (take 7 input))
                     <*> findColumn (take 3 $ drop 7 input)

    calculateSeatID :: Seat -> Int
    calculateSeatID (Seat r c) = r*8 + c

    day05Part1 = (fromJust <$> (maximum . map calculateSeatID <$>)) . (sequence <$> map findSeat) <$> readBoardingPasses
    day05Part2 = (fromJust <$> (findSingleUnoccupiedSeat . findEmptySeats . sort . map calculateSeatID <$>)) . (sequence <$> map findSeat) <$> readBoardingPasses

    allSeatIDsBoard = Set.fromList [8..1015]

    findEmptySeats :: [Int] -> [Int]
    findEmptySeats seats = Set.toList $ allSeatIDsBoard Set.\\ Set.fromList seats

    findSingleUnoccupiedSeat :: [Int] -> Int
    findSingleUnoccupiedSeat [] = -1
    findSingleUnoccupiedSeat [x] = -1
    findSingleUnoccupiedSeat (x : y : xs) | x + 1 < y = y
                                     | otherwise = findSingleUnoccupiedSeat (y:xs)

    readBoardingPasses :: IO [String]
    readBoardingPasses = lines <$> readFile "resource/2020/day05"