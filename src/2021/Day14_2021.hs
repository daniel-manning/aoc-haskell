module Day14_2021 where

import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (many1, letter, Parser, parse, char, string, (<|>))
import Data.Either.Combinators ( fromRight' )
import Data.Maybe ( fromJust )
import Data.List (sort, group, sortBy, groupBy, foldl1)
import Data.Function ( on )

data State = State String Integer deriving Show

parsePairInsertion :: Parser (String, Char)
parsePairInsertion = do
    pair <- many1 letter
    string " -> "
    c <- letter
    return (pair, c)

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day14"
-------------------
readAndParse :: IO (String, Map.Map String Char)
readAndParse = (\x -> (head x, Map.fromList $ map (fromRight' . parse parsePairInsertion "") $ drop 2 x)) <$> readData

runAutomata ::  Map.Map String Char ->String -> String
runAutomata _ [] = []
runAutomata _ [x] = [x]
runAutomata pairMap (x:y:xs) =  x : fromJust (Map.lookup [x,y] pairMap) : runAutomata pairMap (y:xs)

nTimes :: Int -> (c -> c) -> c -> c
nTimes 0 _ = id
nTimes 1 f = f
nTimes n f = f . nTimes (n-1) f


runPt1 :: (String, Map.Map String Char) -> Int
runPt1 = (\xs -> maximum xs - minimum xs) . map length . group . sort . (\l -> nTimes 10 (runAutomata (snd l)) (fst l))
------------------

alterBuckets ::  Map.Map String Char -> [State] ->[State]
alterBuckets _ [] = []
alterBuckets pairMap (s: ss) = State before n : State after n : alterBuckets pairMap ss
    where
        pair = (\(State pair _) -> pair) s
        n = (\(State _ n) -> n) s
        c = fromJust $ Map.lookup pair pairMap
        before = [head pair, c]
        after = c : tail pair

mergeBuckets :: [State] -> [State]
mergeBuckets = map (foldl1 (\(State a n) (State b m) -> State a (n+m))) . groupBy (\(State p1 _) (State p2 _) -> p1 == p2) . sortBy (compare `on` (\(State p _) -> p))

loadBuckets :: String -> [State]
loadBuckets [] = []
loadBuckets [x] = []
loadBuckets (x:y:xs) = State [x,y] 1 : loadBuckets (y:xs)

countBuckets :: [State] -> [(Char, Integer)]
countBuckets [] = []
countBuckets ((State p n):ss) = (head p, n) : (head $ tail p, n) : countBuckets ss

combine :: Num b => (a, b) -> (a, b) -> (a, b)
combine l m = (fst l, snd l + snd m)

countLetterFrequency :: String -> [State] -> [(Char, Integer)]
countLetterFrequency s ss = map (endDoubleCount . foldl1 combine) $ groupBy (\l m -> fst l == fst m) $ sortBy (compare `on` fst) $ countBuckets ss
    where
        endPieces = [head s, last s] 
        endDoubleCount p = if fst p `elem` endPieces then (fst p, (snd p + 1) `div` 2) else (fst p, snd p `div` 2)

maximumMinusMinimum :: [(Char, Integer)] -> Integer 
maximumMinusMinimum xs = (\x -> maximum x - minimum x) $ map snd xs

runPt2 :: (String, Map.Map String Char) -> Integer
runPt2 l = maximumMinusMinimum $ countLetterFrequency (fst l) . nTimes 40 (mergeBuckets . alterBuckets (snd l)) $ mergeBuckets $ loadBuckets (fst l)

solution :: IO Integer
solution =  runPt2 <$> readAndParse
