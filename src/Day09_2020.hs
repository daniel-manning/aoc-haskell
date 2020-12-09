module Day09_2020
    (
    ) where

    import Data.Set (Set)
    import qualified Data.Set as Set

    validate :: Int -> Set Int -> Bool
    validate n numbers = 1 <= (Set.size $ Set.filter (\l -> Set.member (n - l) (Set.delete l numbers)) numbers)

    gatherPreamble :: Int -> [Int] -> (Set Int, Int)
    gatherPreamble n numbers = (Set.fromList $ take n numbers, head $ drop n numbers)

    window :: Int -> [Int] -> [[Int]]
    window n numbers | length numbers < n = [[]]
                     | length numbers == n = [numbers]
                     | otherwise = (take n numbers) : window n (tail numbers)

    findInvalidCode :: Int -> IO Int
    findInvalidCode n = snd . head . filter (\l -> not $ validate (snd l) (fst l)) . map (gatherPreamble n) . (window (n+1)) <$> readNumbers

    day09Pt1 = findInvalidCode 25

    readNumbers :: IO [Int]
    readNumbers = map read . lines <$> readFile "resource/2020/day09"