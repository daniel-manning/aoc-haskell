module Day01_2023 (
) where
import Data.Char (isDigit)
import Data.List.NonEmpty as N (NonEmpty, head, last, tail, fromList, isPrefixOf)
import Text.Read (readMaybe)


readData :: IO [String]
readData = lines <$> readFile "resource/2023/day01"

----------------
calibrationString  = filter isDigit

calibrationValue :: NonEmpty a -> [a] 
calibrationValue xs = [N.head xs, N.last xs]

readCalibrationValue :: String -> Maybe Int
readCalibrationValue = readMaybe . calibrationValue . N.fromList  . calibrationString

runPt1 :: IO (Maybe Int)
runPt1 = fmap sum . mapM readCalibrationValue <$> readData
--------------

firstN :: NonEmpty Char -> Int
firstN xs | isDigit (N.head xs) = read [N.head xs]
          | "one" `isPrefixOf` xs = 1
          | "two" `isPrefixOf` xs = 2
          | "three" `isPrefixOf` xs = 3
          | "four" `isPrefixOf` xs = 4
          | "five" `isPrefixOf` xs = 5
          | "six" `isPrefixOf` xs = 6
          | "seven" `isPrefixOf` xs = 7
          | "eight" `isPrefixOf` xs = 8
          | "nine" `isPrefixOf` xs = 9
          | otherwise = firstN (N.fromList $ N.tail xs)

lastN :: NonEmpty Char -> Int
lastN xs | isDigit (N.head xs) = read [N.head xs]
         | "eno" `isPrefixOf` xs = 1
         | "owt" `isPrefixOf` xs = 2
         | "eerht" `isPrefixOf` xs = 3
         | "ruof" `isPrefixOf` xs = 4
         | "evif" `isPrefixOf` xs = 5
         | "xis" `isPrefixOf` xs = 6
         | "neves" `isPrefixOf` xs = 7
         | "thgie" `isPrefixOf` xs = 8
         | "enin" `isPrefixOf` xs = 9
         | otherwise = lastN (N.fromList $ N.tail xs)

readCalibrationValue' :: String -> Int
readCalibrationValue' xs = 10*firstN (N.fromList xs)  + lastN (N.fromList $ reverse xs)

runPt2 :: IO Int
runPt2 = sum . map readCalibrationValue' <$> readData