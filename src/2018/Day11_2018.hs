module Day11_2018 (
) where

import Data.List (maximumBy)
import Data.Ord (comparing)

type Position = (Int, Int)
type GSN = Int

gsn :: GSN
gsn = 6042
--gsn = 42

rackId :: Position -> Int
rackId (x, _) = x + 10

powerOfCell :: Position -> Int
powerOfCell (x, y) = hundredDigit (r * (gsn + (y * r))) - 5
    where
        r = rackId (x,y)

hundredDigit :: Int -> Int 
hundredDigit hd = (hd `div` 100) `mod` 10

neighbourhood :: Position -> [Position]
neighbourhood (x, y) = [(x + l, y + m) | l <- [0, 1, 2], m <- [0, 1, 2]]

totalPower = sum . map powerOfCell . neighbourhood

largestTotalPower = maximumBy (comparing snd) [(\p -> (p, totalPower p)) (1 + l, 3 + m) | l <- [0 ..297], m <- [0..297]]

