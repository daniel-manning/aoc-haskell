module Day14_2021 where

import qualified Data.Map.Strict as Map
import Text.ParserCombinators.Parsec (many1, letter, Parser, parse, char, string, (<|>))
import Data.Either.Combinators ( fromRight' )
import Data.Maybe ( fromJust )
import Data.List (sort, group)

parsePairInsertion :: Parser (String, Char)
parsePairInsertion = do
    pair <- many1 letter
    string " -> "
    c <- letter
    return (pair, c)

readData :: IO [String]
readData = lines <$> readFile "resource/2021/day14_test"
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

solution :: IO Int
solution =  runPt1 <$> readAndParse