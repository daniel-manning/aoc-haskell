module Day04_2017 (
) where


import Data.List.Split (splitOn)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort)

tokenWords :: String -> [String]
tokenWords = splitOn " "

noRepeats :: Ord a => [a] -> Bool
noRepeats as = length as == Set.size (Set.fromList as)

isValid :: String -> Bool
isValid = noRepeats . tokenWords

readData = lines <$> readFile "resource/2017/day04"

runPt1 = length . filter isValid <$> readData
------------------
isValid' :: String -> Bool
isValid' = noRepeats . map sort . tokenWords

runPt2 = length . filter isValid' <$> readData