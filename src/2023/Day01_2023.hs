module Day01_2023 (
) where
import Data.Char (isDigit)
import Data.List.NonEmpty as N (NonEmpty, head, last, fromList)
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

firstN :: String -> Int
firstN [] = error "No number!"
firstN ('1':_) = 1
firstN ('2':_) = 2
firstN ('3':_) = 3
firstN ('4':_) = 4
firstN ('5':_) = 5
firstN ('6':_) = 6
firstN ('7':_) = 7
firstN ('8':_) = 8
firstN ('9':_) = 9
firstN ('o':'n':'e': _) = 1
firstN ('t':'w':'o' : _) = 2
firstN ('t':'h':'r':'e':'e' : _) = 3
firstN ('f':'o':'u':'r' : _) = 4
firstN ('f':'i':'v':'e' : _) = 5
firstN ('s':'i':'x' : _) = 6
firstN ('s':'e':'v':'e':'n' : _) = 7
firstN ('e':'i':'g':'h':'t' : _) = 8
firstN ('n':'i':'n':'e' : _) =  9
firstN (x: xs) = firstN xs

lastN :: String -> Int
lastN ('1':xs) = 1
lastN ('2':xs) = 2
lastN ('3':xs) = 3
lastN ('4':xs) = 4
lastN ('5':xs) = 5
lastN ('6':xs) = 6
lastN ('7':xs) = 7
lastN ('8':xs) = 8
lastN ('9':xs) = 9
lastN ('e':'n':'o': _) = 1
lastN ('o':'w':'t' : _) = 2
lastN ('e':'e':'r':'h':'t' : _) = 3
lastN ('r':'u':'o':'f' : _) = 4
lastN ('e':'v':'i':'f' : _) = 5
lastN ('x':'i':'s' : _) = 6
lastN ('n':'e':'v':'e':'s' : _) = 7
lastN ('t':'h':'g':'i':'e' : _) = 8
lastN ('e':'n':'i':'n' : _) =  9
lastN (x: xs) = lastN xs

readCalibrationValue' :: String -> Int
readCalibrationValue' xs = 10*firstN xs  + lastN (reverse xs)

runPt2 :: IO Int
runPt2 = sum . map readCalibrationValue' <$> readData