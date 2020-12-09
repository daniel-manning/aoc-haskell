module Day09_2020
    (
    ) where

    import Data.List (delete)

    validate :: Int -> [Int] -> Bool
    validate n numbers = 1 <= (length $ filter (\l ->(n - l) `elem` (delete l numbers)) numbers)

    gatherPreamble :: Int -> [Int] -> ([Int], Int)
    gatherPreamble n numbers = (take n numbers, head $ drop n numbers)


    allContiguousRegions :: [a] -> [[a]]
    allContiguousRegions list = (\n -> window n list) =<< [1..(length list)]

    findRangeForInvalidNumber :: Int -> [Int] -> [Int]
    findRangeForInvalidNumber n numbers = head $ filter (\x -> sum x == n) $ allContiguousRegions numbers

    window :: Int -> [a] -> [[a]]
    window n numbers | length numbers < n = [[]]
                     | length numbers == n = [numbers]
                     | otherwise = (take n numbers) : window n (tail numbers)

    findInvalidCode :: Int -> IO Int
    findInvalidCode n = snd . head . filter (\l -> not $ validate (snd l) (fst l)) . map (gatherPreamble n) . (window (n+1)) <$> readNumbers

    day09Pt1 = findInvalidCode 25

    calculateWeakness :: [Int] -> Int
    calculateWeakness l = (maximum l)+(minimum l)

    searchFileForRegion n = (findRangeForInvalidNumber n) . takeWhile (/= n) <$> readNumbers

    day09Pt2 = calculateWeakness <$> searchFileForRegion 1309761972


    readNumbers :: IO [Int]
    readNumbers = map read . lines <$> readFile "resource/2020/day09"