module Day08 where

import Text.ParserCombinators.Parsec (many1, Parser, parse, sepBy, letter, string, space, count, char)
import Data.Either.Combinators ( fromRight' )
import Data.List (find, sort, intersect, (\\))
import Data.List.Extra (trim)  
import Data.Maybe ( fromJust )

newtype Signal = Signal String deriving (Eq, Show)
data Puzzle = Puzzle [Signal] [Signal] deriving Show

splitInTwain :: [Char] -> Puzzle
splitInTwain = (\xs -> Puzzle (map Signal $ digits xs) (map Signal $ codes xs)) . span (/= '|')
   where
       digits = splitList ' ' . trim . fst
       codes = splitList ' ' . trim . tail . snd

split :: (a -> Bool) -> [a] -> ([a], [a])
split f s = (left, right)
        where
        (left,right')=break f s
        right = if null right' then [] else tail right'

splitList :: Eq a => a -> [a] -> [[a]]
splitList _   [] = []
splitList sep list = h:splitList sep t
        where (h,t)=split (==sep) list

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day08"

readAndParse :: IO [Puzzle]
readAndParse = map splitInTwain <$> readData
---------------
locate :: Int -> [Signal] -> Signal
locate n = fromJust . find (\(Signal s) -> length s == n)

locate4 = locate 4
locate7 = locate 3
locate1 = locate 2
locate8 = locate 7

equals :: Signal -> Signal -> Bool
equals (Signal s) (Signal t) = sort s == sort t

puzzleHas1478 :: Puzzle -> Int
puzzleHas1478 (Puzzle d c) = length $ filter (\sig -> equals f sig || equals s sig || equals o sig || equals e sig) c
    where
        f = locate4 d
        s = locate7 d
        o = locate1 d
        e = locate8 d

runPt1 = sum . map puzzleHas1478
-------------------

{-
0 - 6 - (whats left over?)
1 - 2 - done
2 - 5 - done
3 - 5 - done
4 - 4 - done
5 - 5 (get all length 5 without 3 - intersect with 4 to get 3 segments)
6 - 6  - done
7 - 3 - done
8 - 7 - done
9 - 6 - done
-}

locate3 :: Signal -> [Signal] -> Signal
locate3 (Signal one) = head . filter (\(Signal x) -> length (x `intersect` one) == 2) . filter (\(Signal d) -> length d == 5)

locate2 :: Signal -> [Signal] -> Signal
locate2 (Signal four) = head . filter (\(Signal x) -> length (x `intersect` four) == 2) . filter (\(Signal d) -> length d == 5)

locate6  :: Signal -> [Signal] -> Signal
locate6 (Signal one) = head . filter (\(Signal x) -> length (x `intersect` one) == 1) . filter (\(Signal d) -> length d == 6)

locate9  :: Signal -> [Signal] -> Signal
locate9 (Signal four) = head . filter (\(Signal x) -> length (x `intersect` four) == 4) . filter (\(Signal d) -> length d == 6)

puzzleSolved :: Puzzle -> Int
puzzleSolved (Puzzle d c) = read $ concatMap show $ solve dic c
        where
            d4 = locate4 d
            d7 = locate7 d
            d1 = locate1 d
            d8 = locate8 d
            d3 = locate3 d1 d
            d2 = locate2 d4 d
            d6 = locate6 d1 d
            d9 = locate9 d4 d
            rest = d \\ [d4, d7, d1, d8, d3, d2, d6, d9]
            d5 = head $ filter (\(Signal d) -> length d == 5) rest
            d0 = head $ filter (\(Signal d) -> length d == 6) rest
            dic = [(0,d0), (1,d1), (2,d2), (3,d3), (4,d4), (5,d5), (6,d6), (7,d7), (8,d8), (9,d9)]

solve :: [(Int, Signal)] -> [Signal] -> [Int]
solve _ [] = []
solve dic (x:xs) = fst (fromJust (find (equals x . snd) dic)) : solve dic xs

runPt2 :: [Puzzle] -> Int
runPt2 = sum . map puzzleSolved

solution :: IO Int
solution = runPt2 <$> readAndParse
