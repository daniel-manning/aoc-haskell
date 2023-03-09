module Day06_2017 (
) where

import Text.ParserCombinators.Parsec
import Data.Either.Combinators

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.List (foldl1', unfoldr, elemIndices)


parseMemory :: Parser [Int]
parseMemory = map read <$> sepBy1 (many1 digit) (char ' ' <|> char '\t')

readData :: IO String
readData = readFile "resource/2017/day06"

--readAndParse :: IO [Int]
readAndParse = fromRight' . parse parseMemory "" <$> readData

-------------------------------
createIntMap :: [Int] ->  IntMap.IntMap Int
createIntMap = IntMap.fromAscList . zip [0..] 

redistribute :: IntMap.IntMap Int -> IntMap.IntMap Int
redistribute m = foldl (flip (IntMap.adjust (+1))) (IntMap.adjust (const 0) (fst k) m) $ map (\s -> (s + fst k) `mod` l) [1..(snd k)]
    where
        k = maxValue m
        l = IntMap.size m 


maxValue :: (Ord a) => IntMap.IntMap a -> (Int, a)
maxValue as = foldl1' (\a b -> if snd a >= snd b && fst a <= fst b then a else b ) $ IntMap.toList as

redistributeUntilRepeatedPattern :: (Int, IntMap Int, HashSet (IntMap Int)) -> Maybe (Int, (Int, IntMap Int, HashSet (IntMap Int)))
redistributeUntilRepeatedPattern (n, as, hsas) | HashSet.member as' hsas = Nothing
                                               | otherwise =  Just (n + 1, (n + 1, as', HashSet.insert as' hsas))
    where
        as' = redistribute as


stepsUntilRepeatedPattern :: IntMap.IntMap Int -> Int
stepsUntilRepeatedPattern as = 1 + last (unfoldr redistributeUntilRepeatedPattern (0, as, HashSet.empty))

runPt1 = stepsUntilRepeatedPattern . createIntMap <$> readAndParse
-----------------------------------

redistributeUntilRepeatedPattern' :: (IntMap Int, [IntMap Int]) -> Maybe (IntMap Int, (IntMap Int, [IntMap Int]))
redistributeUntilRepeatedPattern' (as, hsas) | as' `elem` hsas = Nothing
                                               | otherwise =  Just (as', (as', as':hsas))
    where
        as' = redistribute as


allStatesUntilRepeatedPattern :: IntMap.IntMap Int -> [IntMap.IntMap Int]
allStatesUntilRepeatedPattern as = unfoldr redistributeUntilRepeatedPattern' (as, [])

findCycleLength as = 1 + head (elemIndices rep (reverse as))
     where
        rep = redistribute (last as)

runPt2 = findCycleLength . allStatesUntilRepeatedPattern . createIntMap <$> readAndParse