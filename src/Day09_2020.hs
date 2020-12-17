module Day09_2020
    (
    ) where

    import Data.List (delete)
    import ListUtils (window)

    validate :: Int -> [Int] -> Bool
    validate n numbers = not ( null (filter (\l ->(n - l) `elem` (delete l numbers)) numbers))

    gatherPreamble :: Int -> [Int] -> ([Int], Int)
    gatherPreamble n numbers = (take n numbers, numbers !! max 0 n)


    allContiguousRegions :: [a] -> [[a]]
    allContiguousRegions list = (`window` list) =<< [1..(length list)]

    findRangeForInvalidNumber :: Int -> [Int] -> [Int]
    findRangeForInvalidNumber n numbers = head $ filter (\x -> sum x == n) $ allContiguousRegions numbers

    findInvalidCode :: Int -> IO Int
    findInvalidCode n = snd . head . filter (\l -> not $ validate (snd l) (fst l)) . map (gatherPreamble n) . window (n+1) <$> readNumbers

    day09Pt1 = findInvalidCode 25

    calculateWeakness :: [Int] -> Int
    calculateWeakness l = maximum l + minimum l

    searchFileForRegion n = findRangeForInvalidNumber n . takeWhile (/= n) <$> readNumbers

    day09Pt2 = calculateWeakness <$> searchFileForRegion 1309761972


    readNumbers :: IO [Int]
    readNumbers = map read . lines <$> readFile "resource/2020/day09"