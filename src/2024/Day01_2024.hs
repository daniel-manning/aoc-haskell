module Day01_2024 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.Tuple.Extra (both)
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import Data.Bifunctor (second)

newtype LocationID = LocationID Int deriving (Eq, Ord, Show)
----------
parseLocationIDs =  do
        m <- many1 digit
        string "   "
        n <- many1 digit
        return $ (LocationID (read m), LocationID (read n))

readData :: IO [(LocationID, LocationID)]
readData = map (fromRight' . parse parseLocationIDs "") . lines <$> readFile "resource/2024/day01"
----------
twoSortedLists :: [(LocationID, LocationID)] -> ([LocationID], [LocationID])
twoSortedLists = both sort . unzip

calculateAbsoluteDistance :: LocationID -> LocationID -> Int
calculateAbsoluteDistance (LocationID m) (LocationID n) = abs (m - n)

calculateDistances :: ([LocationID], [LocationID]) -> [Int]
calculateDistances  = map (uncurry calculateAbsoluteDistance) . uncurry zip

runPt1 = sum . calculateDistances . twoSortedLists <$> readData
----------
withTotalMap :: [(LocationID, LocationID)] -> ([LocationID], Map.Map LocationID Int)
withTotalMap = second (Map.fromList . map (\x -> (head x, length x)) . group . sort) . unzip

similarityScore :: Map.Map LocationID Int -> LocationID -> Int
similarityScore totalMap lid@(LocationID l) = multiplier * l
    where
        multiplier = Map.findWithDefault 0 lid totalMap

calculateTotalSimilarityScore :: ([LocationID], Map.Map LocationID Int) -> [Int]
calculateTotalSimilarityScore (ls, totalMap) = map (similarityScore totalMap) ls

runPt2 = sum . calculateTotalSimilarityScore . withTotalMap <$> readData