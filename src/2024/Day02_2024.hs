module Day02_2024 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators
import Data.Tuple.Extra (both)
import Data.List (group, sort)
import qualified Data.Map.Strict as Map
import Data.Bifunctor (second)
import Control.Applicative
import Data.Traversable (sequenceA)
import Data.List (tails, nub)

newtype Report = Report [Int] deriving Show
newtype Error = Error Int deriving (Eq, Show)
----------
parseReport :: Parser Report
parseReport = Report . map read <$> sepBy1 (many1 digit) (many1 (char ' '))

readData :: IO [Report]
readData = map (fromRight' . parse parseReport "") . lines <$> readFile "resource/2024/day02"
----------
transpose' :: [[a]] -> [[a]]
transpose' = getZipList . sequenceA . map ZipList

windows :: Int -> [a] -> [[a]]
windows m = transpose' . take m . tails

allIncreasing :: Report -> Bool
allIncreasing (Report rs) = all (\[l, m] -> l < m) $ windows 2 rs 

allDecreasing :: Report -> Bool
allDecreasing (Report rs) = all (\[l, m] -> l > m) $ windows 2 rs 

differBetweenOneAndThree :: [Int] -> Bool
differBetweenOneAndThree [l, m] = blm >= 1 && blm <= 3 
    where
        blm = abs (l - m)

allAdjcentDifferBetweenOneAndThree :: Report -> Bool
allAdjcentDifferBetweenOneAndThree (Report rs) = all differBetweenOneAndThree $ windows 2 rs

validateReport :: Report -> Bool
validateReport r = (allIncreasing r || allDecreasing r) && allAdjcentDifferBetweenOneAndThree r

runPt1 = length . filter id . map validateReport <$> readData
---------
validateAdjactent :: ([Int] -> Bool) -> Report -> Either [Error] ()
validateAdjactent f (Report rs) =  case increases of
    [] -> Right ()
    xs -> Left (map (Error . fst) xs)
    where
        increases = filter (not.snd) $ zip [1..] $ map f $ windows 2 rs

validateAllIncreasing = validateAdjactent (\[l, m] -> l < m)
validateAllDecreasing = validateAdjactent (\[l, m] -> l > m)
validateAllAdjcentDifferBetweenOneAndThree = validateAdjactent differBetweenOneAndThree

chooseSmallestErrorSet ea eb | length ea < length eb = ea
                             | otherwise = eb

validateReport' r = case (inc, dec, diff) of
     ((Right _), _, Right _) -> Right ()
     (_, (Right _), Right _) -> Right ()
     (Left ei, Left ed, Right _) -> Left (nub (chooseSmallestErrorSet ei ed))
     ((Right _), _, Left edf) -> Left edf
     (_, (Right _), Left edf) -> Left edf
     (Left ei, Left ed, Left edf) -> Left (nub ((chooseSmallestErrorSet ei ed) ++ edf))
    where
        inc = validateAllIncreasing r
        dec = validateAllDecreasing r
        diff = validateAllAdjcentDifferBetweenOneAndThree r

deleteAt :: Int -> [a] -> [a]
deleteAt n as = init r ++ rs
    where
        (r, rs) = splitAt n as

deleteBadData [Error n] r = deleteAt n r


isFixable :: Report -> Bool
isFixable rep@(Report r) = case res of
    Right () -> True
    Left es -> case es of
        [Error n] -> 
            if n == length r then validateReport (Report (deleteBadData [Error n] r))
            else if n == 1 then validateReport (Report (deleteBadData [Error 1] r)) || validateReport (Report (deleteBadData [Error 2] r))
            else validateReport (Report (deleteBadData [Error (n-1)] r)) || validateReport (Report (deleteBadData [Error n] r)) || validateReport (Report (deleteBadData [Error (n+1)] r))
        [Error n, Error m] -> validateReport (Report (deleteBadData [Error n] r)) || validateReport (Report (deleteBadData [Error m] r))
        otherwise -> False 
    where
        res = validateReport' rep

runPt2 = length . filter id . map isFixable <$> readData